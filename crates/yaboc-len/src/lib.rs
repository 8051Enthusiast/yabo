#![allow(mixed_script_confusables)]
pub mod prob_array;
pub mod regex;
mod represent;

use depvec::{ArgDeps, DepVec, IndexDepVec, SmallBitVec};
pub use represent::len_graph;

use prob_array::{RandomArray, UNINIT};
use smallvec::SmallVec;
use std::{cmp::Ordering, hash::Hash, slice, sync::Arc};
use yaboc_req::{NeededBy, RequirementMatrix, RequirementSet};

pub mod depvec;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PolyOp {
    Mul([usize; 2]),
    Add([usize; 2]),
    Neg(usize),
}

impl PolyOp {
    pub fn ref_indices(&self) -> &[usize] {
        match self {
            PolyOp::Mul(x) | PolyOp::Add(x) => x,
            PolyOp::Neg(x) => slice::from_ref(x),
        }
    }
    fn remap_indices(&self, index: &[usize]) -> Self {
        match self {
            PolyOp::Mul([a, b]) => PolyOp::Mul([index[*a], index[*b]]),
            PolyOp::Add([a, b]) => PolyOp::Add([index[*a], index[*b]]),
            PolyOp::Neg(a) => PolyOp::Neg(index[*a]),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum PolyGate {
    Const(i128),
    Arg(u32),
    CapturedArg(u32),
    Op(PolyOp),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PolyCircuit {
    pub gates: Vec<PolyGate>,
    pub arg_deps: IndexDepVec,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum ScopeKind {
    PartialLenUse,
    AllValUse,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ScopeInfoIdx(u32);

impl ScopeInfoIdx {
    pub fn new(idx: u32) -> Self {
        Self(idx)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ScopeInfo {
    pub captures: SmallBitVec,
    pub kind: ScopeKind,
    pub params: SmallVec<[ArgRank; 4]>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Term<ParserRef> {
    Pd(ParserRef),
    Arg(u32),
    Apply([usize; 2]),
    Const(i128),
    Opaque,
    OpaqueUn(usize),
    OpaqueBin([usize; 2]),
    Mul([usize; 2]),
    Add([usize; 2]),
    Neg(usize),
    Arr,
    Unify([usize; 2]),
    UnifyDyn([usize; 2]),
    Then([usize; 2]),
    Copy(usize),
    Size(bool, usize),
    ScopeIntro(ScopeInfoIdx),
    BlockEnd(ScopeInfoIdx, usize),
    FunctionEnd(ScopeInfoIdx, usize),
    Parsed,
    Span,
    Cat([usize; 2]),
    Backtracking(usize),
}

impl<ParserRef> Term<ParserRef> {
    fn ref_indices(&self) -> &[usize] {
        match self {
            Term::Pd(_)
            | Term::Arg(_)
            | Term::Const(_)
            | Term::Opaque
            | Term::Arr
            | Term::Parsed
            | Term::ScopeIntro(_)
            | Term::Span => &[],
            Term::OpaqueUn(x)
            | Term::Neg(x)
            | Term::Copy(x)
            | Term::Size(_, x)
            | Term::BlockEnd(_, x)
            | Term::FunctionEnd(_, x)
            | Term::Backtracking(x) => slice::from_ref(x),
            Term::OpaqueBin(x)
            | Term::Apply(x)
            | Term::Unify(x)
            | Term::UnifyDyn(x)
            | Term::Mul(x)
            | Term::Add(x)
            | Term::Cat(x)
            | Term::Then(x) => x,
        }
    }
}

#[derive(Default, PartialEq, Eq, Debug, Clone)]
pub struct SizeExpr<Pd> {
    pub terms: Vec<Term<Pd>>,
    pub arg_depth: Vec<u32>,
    pub scope_info: SmallVec<[ScopeInfo; 2]>,
}

impl<Pd> SizeExpr<Pd> {
    pub fn new() -> Self {
        Self {
            terms: vec![],
            arg_depth: vec![],
            scope_info: Default::default(),
        }
    }

    pub fn push(&mut self, term: Term<Pd>, arg_depth: u32) -> usize {
        self.terms.push(term);
        self.arg_depth.push(arg_depth);
        self.terms.len() - 1
    }
    fn for_each_dep<T: Clone + std::fmt::Debug>(
        &self,
        term_idx: usize,
        vals: &[Val<T>],
        mut f: impl FnMut(&Self, usize, RequirementMatrix),
    ) {
        let term = &self.terms[term_idx];
        match term {
            Term::Cat([lhs, rhs]) | Term::UnifyDyn([lhs, rhs]) => {
                // this is only used for len functions to statically
                // calculate their size and therefore only needs the length
                let req = RequirementMatrix::diag(!!NeededBy::Len);
                f(self, *lhs, req);
                f(self, *rhs, req);
            }
            Term::Copy(inner) => {
                // note that Backtrack is absent as it doesn't propagate accross expressions
                let req = RequirementMatrix::diag(NeededBy::Len | NeededBy::Val);
                f(self, *inner, req);
            }
            Term::Unify([lhs, rhs]) => {
                // unlike other general ops, this one allows parsers
                // to be passed through
                let req = RequirementMatrix::id();
                // the result also depends on backtracking of the left side
                let lreq = req
                    | RequirementMatrix::outer(
                        !!NeededBy::Backtrack,
                        NeededBy::Len | NeededBy::Val,
                    );
                f(self, *lhs, lreq);
                f(self, *rhs, req);
            }
            Term::Size(_, inner) => {
                // when we want the size val of something, we only need the length
                // of the argument
                let req = RequirementMatrix::diag(NeededBy::Len | NeededBy::Backtrack)
                    | RequirementMatrix::outer(!!NeededBy::Len, !!NeededBy::Val);
                f(self, *inner, req);
            }
            Term::Apply([lhs, rhs]) => {
                f(self, *lhs, RequirementMatrix::id());
                let val = &vals[*lhs];
                // when we evaluate the value, we call the function with all its values,
                // but when we want the length, we may not need everything
                let mut req = RequirementMatrix::diag(NeededBy::Val | NeededBy::Backtrack);
                req |= RequirementMatrix::outer(val.next_arg_uses(), !!NeededBy::Len);
                f(self, *rhs, req);
            }
            Term::Then([lhs, rhs]) => {
                // the value on the left side is ignored
                let lreq = RequirementMatrix::diag(!!NeededBy::Backtrack);
                f(self, *lhs, lreq);
                let rreq = RequirementMatrix::id();
                f(self, *rhs, rreq);
            }
            Term::BlockEnd(scope_idx, len) | Term::FunctionEnd(scope_idx, len) => {
                // note that we may iterate over the same value with different matrices twice
                let scope_info = &self.scope_info[scope_idx.0 as usize];
                let kind = scope_info.kind;
                // if this is an inline block, we evaluate the whole block as an opaque
                // value which needs all dependencies even in case we only want the len
                let needs_val = match kind {
                    ScopeKind::PartialLenUse => !!NeededBy::Val,
                    ScopeKind::AllValUse => NeededBy::Len | NeededBy::Val,
                };
                let vreq = RequirementMatrix::outer(!!NeededBy::Val, needs_val);
                for cap in scope_info.captures.iter_ones() {
                    f(self, cap, vreq);
                }
                let lreq = RequirementMatrix::diag(!!NeededBy::Len);
                f(self, *len, lreq);
            }
            Term::Backtracking(inner) => {
                // in case of backtracking, whether it backtracks depends on the value
                // dependencies
                let mut req = RequirementMatrix::id();
                req |= RequirementMatrix::outer(!!NeededBy::Val, !!NeededBy::Backtrack);
                f(self, *inner, req);
            }
            Term::Parsed
            | Term::Span
            | Term::Arg(_)
            | Term::Pd(_)
            | Term::Const(_)
            | Term::ScopeIntro(_)
            | Term::Opaque
            | Term::OpaqueUn(_)
            | Term::OpaqueBin(_)
            | Term::Mul(_)
            | Term::Add(_)
            | Term::Neg(_)
            | Term::Arr => {
                // these operations don't allow parsers to be passed through (or are niladic)
                let req = RequirementMatrix::diag(NeededBy::Backtrack | NeededBy::Val);
                for dep in term.ref_indices() {
                    f(self, *dep, req);
                }
            }
        }
    }
    pub fn reqs<T: Clone + std::fmt::Debug>(
        &self,
        root: usize,
        vals: &[Val<T>],
    ) -> Vec<RequirementSet> {
        let mut idx = root + 1;
        let mut reqs = vec![RequirementSet::empty(); idx];
        reqs[root] = NeededBy::Len.into();
        while idx > 0 {
            idx -= 1;
            self.for_each_dep(idx, vals, |_, dep, mat| {
                let r = reqs[idx];
                reqs[dep] |= mat * r;
            });
        }
        reqs
    }
}

pub enum Fun<ParserRef> {
    ParserRef(ParserRef),
    Static(depvec::SmallBitVec),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Val<PolyCircuitId> {
    Undefined,
    Const(u32, i128, IndexDepVec),
    Arg(u32, u32),
    PolyOp(PolyOp),
    Poly(u32, PolyCircuitId, IndexDepVec),
    PartialPolyApply(u32, [usize; 2], PolyCircuitId, IndexDepVec),
    Static(u32, IndexDepVec),
    Unsized,
}

impl<PolyCircuitId: Clone + std::fmt::Debug> Val<PolyCircuitId> {
    pub fn is_unsized(&self) -> bool {
        matches!(self, Val::Unsized)
    }

    fn is_poly(&self) -> bool {
        matches!(
            self,
            Val::Poly(..)
                | Val::Const(..)
                | Val::Arg(_, _)
                | Val::PolyOp(..)
                | Val::PartialPolyApply(..)
        )
    }

    fn is_poly_fun(&self) -> bool {
        matches!(
            self,
            Val::Poly(1.., ..)
                | Val::Const(1.., ..)
                | Val::PartialPolyApply(1.., ..)
                | Val::Arg(1.., ..)
        )
    }

    pub fn rank(&self) -> Option<u32> {
        match self {
            Val::Undefined => None,
            Val::PolyOp(..) => Some(0),
            Val::Const(rank, ..)
            | Val::Arg(rank, ..)
            | Val::Poly(rank, ..)
            | Val::PartialPolyApply(rank, ..)
            | Val::Static(rank, ..) => Some(*rank),
            Val::Unsized => None,
        }
    }

    fn as_arg_fun(&self) -> Self {
        match self {
            Val::Static(rank, deps)
            | Val::Poly(rank, .., deps)
            | Val::PartialPolyApply(rank, .., deps)
            | Val::Const(rank, _, deps)
                if !deps.is_zero_below(*rank as usize) =>
            {
                Val::Unsized
            }
            otherwise => otherwise.clone(),
        }
    }

    pub fn next_arg_uses(&self) -> RequirementSet {
        match self {
            Val::Undefined | Val::Arg(_, _) | Val::PolyOp(_) => RequirementSet::empty(),
            Val::Unsized => NeededBy::Val | NeededBy::Len,
            Val::Const(rank, _, deps)
            | Val::Poly(rank, _, deps)
            | Val::PartialPolyApply(rank, _, _, deps)
            | Val::Static(rank, deps) => deps.req(*rank as usize - 1),
        }
    }

    fn deps(&self) -> depvec::IndexDepVec {
        match self {
            Val::Arg(_, _) => DepVec::default(),
            Val::Const(_, _, x)
            | Val::Poly(.., x)
            | Val::PartialPolyApply(.., x)
            | Val::Static(_, x) => x.clone(),
            Val::PolyOp(_) | Val::Undefined | Val::Unsized => DepVec::default(),
        }
    }

    pub fn ref_indices(&self) -> &[usize] {
        match self {
            Val::PolyOp(PolyOp::Mul(x) | PolyOp::Add(x)) | Val::PartialPolyApply(_, x, ..) => x,
            Val::PolyOp(PolyOp::Neg(x)) => slice::from_ref(x),
            _ => &[],
        }
    }

    fn fun_hierarchy_rank(&self) -> u8 {
        match self {
            Val::Undefined => 0,
            Val::Const(..) => 1,
            Val::Poly(..) => 2,
            Val::Static(..) => 3,
            Val::Unsized => 4,
            Val::Arg(..) | Val::PolyOp(..) | Val::PartialPolyApply(..) => {
                panic!("fun_hierarchy_rank called on unfun value {:?}", self)
            }
        }
    }

    pub fn has_expanded_from(&self, other: &Self) -> bool {
        match self.fun_hierarchy_rank().cmp(&other.fun_hierarchy_rank()) {
            Ordering::Less => panic!("cannot shrink during absint: {:?} < {:?}", self, other),
            Ordering::Equal => false,
            Ordering::Greater => true,
        }
    }
}

#[derive(Clone, Copy)]
enum Parameter {
    Arg(usize),
    Val(usize),
}

impl Default for Parameter {
    fn default() -> Self {
        Parameter::Arg(0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ArgRank(pub u32);

pub struct Vals<ParserRef> {
    pub vals: Vec<Val<ParserRef>>,
    pub deps: Vec<ArgDeps>,
}
pub trait Env: Sized {
    type ParserRef: Copy + Eq + std::fmt::Debug;
    type PolyCircuitId: Copy + Eq + std::fmt::Debug;

    fn size_info(&self, pd: &Self::ParserRef) -> Val<Self::PolyCircuitId>;

    fn lookup_circuit(&self, id: Self::PolyCircuitId) -> Arc<PolyCircuit>;
    fn intern_circuit(&self, circuit: Arc<PolyCircuit>) -> Self::PolyCircuitId;

    fn arr_circuit(&self) -> Self::PolyCircuitId {
        let arr_circuit = PolyCircuit {
            arg_deps: DepVec::array_deps(),
            gates: vec![
                PolyGate::Arg(0),
                PolyGate::Arg(1),
                PolyGate::Op(PolyOp::Mul([0, 1])),
            ],
        };
        self.intern_circuit(Arc::new(arr_circuit))
    }
}

pub struct SizeCalcCtx<'a, Γ: Env> {
    env: &'a Γ,
    size_expr: &'a SizeExpr<Γ::ParserRef>,
    poly_vals: Vec<RandomArray>,
    pub vals: Vec<Val<Γ::PolyCircuitId>>,
    arr_circuit: Γ::PolyCircuitId,
    args: Vec<ArgRank>,
    arg_poly_vals: Vec<RandomArray>,
    deps: Vec<ArgDeps>,
}

impl<'a, Γ: Env> SizeCalcCtx<'a, Γ> {
    pub fn new(env: &'a Γ, size_expr: &'a SizeExpr<Γ::ParserRef>) -> Self {
        let arg_poly_vals = Vec::new();
        let arr_circuit = env.arr_circuit();
        let mut ret = Self {
            env,
            poly_vals: vec![UNINIT; size_expr.terms.len()],
            vals: Vec::with_capacity(size_expr.terms.len()),
            size_expr,
            arr_circuit,
            args: Vec::new(),
            arg_poly_vals,
            deps: vec![Default::default(); size_expr.terms.len()],
        };
        ret.eval();
        ret
    }

    fn for_each_dep(&self, term_idx: usize, mut f: impl FnMut(&Self, usize, RequirementMatrix)) {
        self.size_expr
            .for_each_dep(term_idx, &self.vals, |_, dep, mat| {
                f(self, dep, mat);
            })
    }

    fn poly_args_impl(&mut self, fun: Val<Γ::PolyCircuitId>) -> Vec<Option<Parameter>> {
        match fun {
            Val::PartialPolyApply(_, [fun, arg], ..) => {
                let mut args = self.poly_args_impl(self.vals[fun].clone());
                if !self.vals[fun].next_arg_uses().is_empty() {
                    args.push(Some(Parameter::Val(arg)));
                } else {
                    args.push(None);
                }
                args
            }
            Val::Poly(rank, ..) => Vec::with_capacity(rank as usize),
            _ => panic!("not a polynomial"),
        }
    }

    fn poly_args(&mut self, idx: usize) -> Vec<Option<Parameter>> {
        let fun = &self.vals[idx];
        let mut args = self.poly_args_impl(fun.clone());
        let fun = &self.vals[idx];
        let rank = match fun {
            Val::PartialPolyApply(rank, ..) => rank,
            Val::Poly(rank, ..) => rank,
            _ => panic!("not a polynomial"),
        };
        let arg_count = self.size_expr.arg_depth[idx] as usize;
        for i in arg_count..(arg_count + *rank as usize) {
            args.push(Some(Parameter::Arg(i)));
        }
        args
    }

    fn get_polyval_arg(&mut self, arg: usize) -> &RandomArray {
        if arg < self.arg_poly_vals.len() {
            return &self.arg_poly_vals[arg];
        }
        self.arg_poly_vals.resize_with(arg + 1, || {
            let mut arr = UNINIT;
            arr.random_element();
            arr
        });
        &self.arg_poly_vals[arg]
    }

    fn eval_circuit(&mut self, circuit: &PolyCircuit, params: &[Option<Parameter>]) -> RandomArray {
        let mut poly_vals = vec![UNINIT; circuit.gates.len()];
        for (idx, gate) in circuit.gates.iter().enumerate() {
            let val = &mut poly_vals[idx];
            match gate {
                PolyGate::Const(c) => val.array_from_const(*c),
                PolyGate::Arg(a) => match params[*a as usize] {
                    Some(Parameter::Arg(a)) => val.clone_from(self.get_polyval_arg(a)),
                    Some(Parameter::Val(v)) => {
                        self.eval_poly(v);
                        val.clone_from(&self.poly_vals[v])
                    }
                    None => panic!("used arg that should not be used"),
                },
                PolyGate::CapturedArg(a) => {
                    val.clone_from(self.get_polyval_arg(*a as usize));
                }
                PolyGate::Op(op) => {
                    let (prev, [val, ..]) = poly_vals.split_at_mut(idx) else {
                        unreachable!()
                    };
                    match op {
                        PolyOp::Add([lhs, rhs]) => {
                            val.add_array(&prev[*lhs], &prev[*rhs]);
                        }
                        PolyOp::Mul([lhs, rhs]) => {
                            val.mul_array(&prev[*lhs], &prev[*rhs]);
                        }
                        PolyOp::Neg(dep) => {
                            val.neg_array(&prev[*dep]);
                        }
                    }
                }
            }
        }
        let [.., res] = poly_vals[..] else {
            panic!("empty circuit")
        };
        res
    }

    fn eval_poly(&mut self, idx: usize) {
        if !self.poly_vals[idx].is_uninit() {
            return;
        }
        match self.vals[idx].clone() {
            Val::Const(_, c, _) => self.poly_vals[idx].array_from_const(c),
            Val::Arg(_, a) => {
                self.poly_vals[idx] = *self.get_polyval_arg(a as usize);
            }
            Val::PolyOp(op) => {
                let deps = match &op {
                    PolyOp::Add(deps) | PolyOp::Mul(deps) => &deps[..],
                    PolyOp::Neg(dep) => slice::from_ref(dep),
                };
                for dep in deps {
                    self.eval_poly(*dep);
                }
                let (prev, [val, ..]) = self.poly_vals.split_at_mut(idx) else {
                    unreachable!()
                };
                match &op {
                    PolyOp::Add([lhs, rhs]) => {
                        val.add_array(&prev[*lhs], &prev[*rhs]);
                    }
                    PolyOp::Mul([lhs, rhs]) => {
                        val.mul_array(&prev[*lhs], &prev[*rhs]);
                    }
                    PolyOp::Neg(dep) => {
                        val.neg_array(&prev[*dep]);
                    }
                }
            }
            Val::Poly(.., circ, _) | Val::PartialPolyApply(.., circ, _) => {
                let args = self.poly_args(idx);
                let circ = self.env.lookup_circuit(circ);
                self.poly_vals[idx] = self.eval_circuit(&circ, &args);
            }
            _ => panic!("attempt to evaluate non-polynomial value"),
        }
    }

    fn poly_op<const N: usize>(
        &self,
        deps: [usize; N],
        exec_op: impl Fn([i128; N]) -> (i128, bool),
        poly_op: PolyOp,
    ) -> Val<Γ::PolyCircuitId> {
        for (predicate, val) in [
            (
                (|x| matches!(x, Val::Undefined)) as fn(&Val<_>) -> _,
                Val::Undefined,
            ),
            (|x| matches!(x, Val::Unsized), Val::Unsized),
            (
                |x| matches!(x, Val::Static(0, _)),
                Val::Static(0, DepVec::default()),
            ),
        ] {
            if deps.iter().any(|x| predicate(&self.vals[*x])) {
                return val;
            }
        }
        let Some(consts) = deps
            .iter()
            .map(|x| match &self.vals[*x] {
                Val::Const(0, x, _) => Some(*x),
                _ => None,
            })
            .collect::<Option<SmallVec<[i128; N]>>>()
        else {
            return Val::PolyOp(poly_op);
        };
        let (res, overflow) = exec_op(consts.as_slice().try_into().unwrap());
        if overflow {
            Val::Undefined
        } else {
            Val::Const(0, res, DepVec::default())
        }
    }

    fn apply_fun(&self, fun: usize, arg: usize) -> Val<Γ::PolyCircuitId> {
        let argv = self.vals[arg].as_arg_fun();
        let next_arg_uses = self.vals[fun].next_arg_uses();
        let uses_len = next_arg_uses.contains(NeededBy::Len);
        let uses_val = next_arg_uses.contains(NeededBy::Val);
        match (&self.vals[fun], &argv, (uses_len, uses_val)) {
            (Val::Undefined, ..) | (_, Val::Undefined, (true, _)) => Val::Undefined,
            (Val::Arg(rank @ 1.., a), ..) => Val::Arg(*rank - 1, *a),
            (Val::Const(rank @ 1.., c, deps), ..) => Val::Const(*rank - 1, *c, deps.clone()),
            (Val::Static(1.., ..), Val::Unsized, (true, _)) => Val::Unsized,
            // because of the previous branch, either the argument is sized
            // or the argument length is not used
            (Val::Static(rank @ 1.., deps), ..) => {
                if *rank == 1 {
                    Val::Static(*rank - 1, DepVec::default())
                } else {
                    Val::Static(*rank - 1, deps.clone())
                }
            }
            (
                Val::PartialPolyApply(rank @ 1.., _, circ, deps)
                | Val::Poly(rank @ 1.., circ, deps),
                argv,
                (true, _),
            ) => {
                if argv.is_unsized() {
                    Val::Unsized
                } else if argv.is_poly() {
                    Val::PartialPolyApply(*rank - 1, [fun, arg], *circ, deps.clone())
                } else {
                    Val::Static(*rank - 1, deps.clone())
                }
            }
            (
                Val::PartialPolyApply(rank @ 1.., _, circ, deps)
                | Val::Poly(rank @ 1.., circ, deps),
                argv,
                (false, true),
            ) => {
                if argv.is_unsized() {
                    Val::Static(*rank - 1, deps.clone())
                } else {
                    Val::PartialPolyApply(*rank - 1, [fun, arg], *circ, deps.clone())
                }
            }
            (
                Val::PartialPolyApply(rank @ 1.., _, circ, deps)
                | Val::Poly(rank @ 1.., circ, deps),
                _,
                (false, false),
            ) => Val::PartialPolyApply(*rank - 1, [fun, arg], *circ, deps.clone()),

            _ => Val::Unsized,
        }
    }

    fn unify(
        &mut self,
        ops @ [lidx, ridx]: [usize; 2],
        dy: bool,
        pos: usize,
    ) -> Val<Γ::PolyCircuitId> {
        let mark_dyn = |this: &mut Self| {
            if dy {
                let arg = this.size_expr.arg_depth[pos] - 1;
                this.deps[pos].len().set_val(arg as usize);
            }
        };
        match ops.map(|x| self.vals[x].clone()) {
            [Val::Undefined, other] | [other, Val::Undefined] => other,
            [Val::Unsized, _] | [_, Val::Unsized] => Val::Unsized,
            [lhs, rhs] if lhs.rank() != rhs.rank() => Val::Unsized,
            [Val::Static(rank, ldeps), Val::Static(_, rdeps)] => {
                let deps = ldeps.or(&rdeps);
                mark_dyn(self);
                Val::Static(rank, deps)
            }
            [Val::Static(rank, ldeps), Val::Poly(.., x) | Val::PartialPolyApply(.., x)]
            | [Val::Poly(.., x) | Val::PartialPolyApply(.., x), Val::Static(rank, ldeps)] => {
                let deps = ldeps.or(&x);
                mark_dyn(self);
                Val::Static(rank, deps)
            }
            [Val::Static(rank, deps), Val::Arg(..) | Val::Const(..) | Val::PolyOp(..)]
            | [Val::PolyOp(..) | Val::Arg(..) | Val::Const(..), Val::Static(rank, deps)] => {
                mark_dyn(self);
                Val::Static(rank, deps)
            }
            [Val::Const(rank, a, ldeps), Val::Const(_, b, rdeps)] => {
                let deps = ldeps.or(&rdeps);
                if a == b {
                    Val::Const(rank, a, deps)
                } else {
                    mark_dyn(self);
                    Val::Static(rank, deps)
                }
            }
            [Val::Arg(rank, a), Val::Arg(_, b)] => {
                if a == b {
                    Val::Arg(rank, a)
                } else {
                    mark_dyn(self);
                    Val::Static(rank, Default::default())
                }
            }
            [lhs @ (Val::PolyOp(..)
            | Val::Poly(..)
            | Val::PartialPolyApply(..)
            | Val::Arg(..)
            | Val::Const(..)), Val::PolyOp(..)
            | Val::Poly(..)
            | Val::PartialPolyApply(..)
            | Val::Arg(..)
            | Val::Const(..)] => {
                _ = ops.map(|x| self.eval_poly(x));
                if self.poly_vals[lidx] == self.poly_vals[ridx] {
                    lhs
                } else {
                    let rank = lhs.rank().unwrap();
                    let deps = lhs.deps();
                    mark_dyn(self);
                    Val::Static(rank, deps)
                }
            }
        }
    }

    fn block_end(&mut self, len_loc: usize) -> Val<Γ::PolyCircuitId> {
        let arg = self.size_expr.arg_depth[len_loc] as usize - 1;
        let lendeps = self.deps[len_loc].len();
        let depends_on_arg = lendeps.has_len(arg) || lendeps.has_val(arg);
        let val = &self.vals[len_loc];
        if depends_on_arg {
            Val::Unsized
        } else {
            val.clone()
        }
    }

    fn function_end(
        &mut self,
        ret_idx: usize,
        scope: ScopeInfo,
        arg_depth: u32,
    ) -> Val<Γ::PolyCircuitId> {
        let arg_count = scope.params.len() as u32;
        let mut val_deps = DepVec::default();
        if scope.kind == ScopeKind::AllValUse {
            for i in 0..arg_count as usize {
                val_deps.set_val(i);
            }
        }
        match &self.vals[ret_idx] {
            Val::Undefined => Val::Undefined,
            Val::Const(0, c, _) => Val::Const(arg_count, *c, val_deps),
            Val::Arg(0, _) | Val::PolyOp(_) | Val::Poly(0, ..) | Val::PartialPolyApply(0, ..) => {
                let circuit = self
                    .create_polycircuit(ret_idx, arg_count, arg_depth)
                    .unwrap();
                let deps = circuit.arg_deps.or(&val_deps);
                let circuit_id = self.env.intern_circuit(Arc::new(circuit));
                Val::Poly(arg_count, circuit_id, deps)
            }
            Val::Static(0, _) => {
                let (_, arg_deps) = self.deps[ret_idx]
                    .len()
                    .split_at(self.args.len(), arg_count as usize);
                let deps = arg_deps.or(&val_deps);
                Val::Static(arg_count, deps)
            }
            Val::Unsized => Val::Unsized,
            // we have a higher order function
            _ => Val::Unsized,
        }
    }

    fn update_deps(&mut self, pos: usize) {
        let term = &self.size_expr.terms[pos];
        let mut cur = self.deps[pos].clone();
        self.for_each_dep(pos, |this, dep, req| {
            cur |= &req.pullback(&this.deps[dep].0);
        });
        self.deps[pos] = cur;
        match term {
            Term::Arg(a) => {
                self.deps[pos].len().set_len(*a as usize);
                self.deps[pos].val().set_val(*a as usize);
            }
            Term::Parsed | Term::Span => {
                let idx = self.size_expr.arg_depth[pos] as usize - 1;
                self.deps[pos].len().set_len(idx);
                self.deps[pos].val().set_val(idx);
            }
            Term::BlockEnd(_, _) | Term::FunctionEnd(_, _) => {
                for dep in self.deps[pos].0.iter_mut() {
                    let depth = self.size_expr.arg_depth[pos] as usize;
                    dep.truncate(depth);
                }
            }
            _ => (),
        }
    }

    fn eval(&mut self) {
        for term_idx in 0..self.size_expr.terms.len() {
            let term = self.size_expr.terms[term_idx];
            let val = match &term {
                Term::Pd(pd) => self.env.size_info(pd),
                Term::Arg(a) => Val::Arg(self.args[*a as usize].0, *a),
                Term::Apply([fun, arg]) => self.apply_fun(*fun, *arg),
                Term::Const(c) => Val::Const(0, *c, Default::default()),
                Term::OpaqueBin(..)
                | Term::Opaque
                | Term::Parsed
                | Term::Span
                | Term::OpaqueUn(_) => Val::Unsized,
                Term::Mul(ops) => self.poly_op(
                    *ops,
                    |[lhs, rhs]| lhs.overflowing_mul(rhs),
                    PolyOp::Mul(*ops),
                ),
                Term::Add(ops) | Term::Cat(ops) => self.poly_op(
                    *ops,
                    |[lhs, rhs]| lhs.overflowing_add(rhs),
                    PolyOp::Add(*ops),
                ),
                Term::Neg(arg) => {
                    self.poly_op([*arg], |[x]| x.overflowing_neg(), PolyOp::Neg(*arg))
                }
                Term::Arr => Val::Poly(2, self.arr_circuit, DepVec::array_deps()),
                Term::Unify(ops) => self.unify(*ops, false, term_idx),
                Term::UnifyDyn(ops) => self.unify(*ops, true, term_idx),
                Term::Size(_, inner) => match self.vals[*inner].clone() {
                    Val::Unsized | Val::Static(_, _) => Val::Unsized,
                    otherwise => otherwise,
                },
                Term::ScopeIntro(scope) => {
                    let info = &self.size_expr.scope_info[scope.0 as usize];
                    self.args.extend_from_slice(&info.params);
                    Val::Undefined
                }
                Term::BlockEnd(idx, len_loc) => {
                    let info = &self.size_expr.scope_info[idx.0 as usize];
                    self.args.truncate(self.args.len() - info.params.len());
                    if info.kind == ScopeKind::PartialLenUse {
                        self.block_end(*len_loc)
                    } else {
                        self.vals[*len_loc].clone()
                    }
                }
                Term::FunctionEnd(idx, len_loc) => {
                    let info = &self.size_expr.scope_info[idx.0 as usize];
                    let arg_depth = self.size_expr.arg_depth[term_idx];
                    let res = self.function_end(*len_loc, info.clone(), arg_depth);
                    self.args.truncate(self.args.len() - info.params.len());
                    res
                }
                Term::Copy(val) | Term::Then([_, val]) | Term::Backtracking(val) => {
                    self.vals[*val].clone()
                }
            };
            self.vals.push(val);
            self.update_deps(term_idx);
        }
    }

    fn include_circuit(
        &self,
        included_circuit: &PolyCircuit,
        new_gates: &mut Vec<PolyGate>,
        term_indices: &[usize],
        args: &[Option<Parameter>],
        arg_depth: u32,
    ) -> usize {
        let mut index = Vec::new();
        for gate in included_circuit.gates.iter() {
            match gate {
                PolyGate::Const(c) => {
                    index.push(new_gates.len());
                    new_gates.push(PolyGate::Const(*c));
                }
                PolyGate::Arg(a) => match args[*a as usize] {
                    Some(Parameter::Arg(a)) => {
                        index.push(new_gates.len());
                        new_gates.push(PolyGate::Arg(a as u32));
                    }
                    Some(Parameter::Val(v)) => {
                        index.push(term_indices[v]);
                    }
                    None => panic!("using parameter that should not be used"),
                },
                PolyGate::CapturedArg(argument) => {
                    index.push(new_gates.len());
                    match argument.checked_sub(arg_depth) {
                        // if the captured argument is >= arg_depth, then it is actually
                        // an argument of the function it is included into
                        Some(a) => new_gates.push(PolyGate::Arg(a)),
                        // otherwise it is a transitive capture
                        None => new_gates.push(PolyGate::CapturedArg(*argument)),
                    }
                }
                PolyGate::Op(op) => {
                    index.push(new_gates.len());
                    let remapped = op.remap_indices(&index);
                    new_gates.push(PolyGate::Op(remapped));
                }
            }
        }
        *index.last().unwrap()
    }

    fn create_polycircuit(
        &mut self,
        root: usize,
        rank: u32,
        arg_depth: u32,
    ) -> Option<PolyCircuit> {
        if !self.vals.get(root).is_some_and(|x| x.is_poly()) {
            return None;
        }
        let mut included = vec![false; self.vals.len()];
        included[root] = true;
        for (i, val) in self.vals.iter().enumerate().rev() {
            if !included[i] {
                continue;
            }
            if val.is_poly_fun() {
                // higher order functions are not directly included
                // in the circuit but flattened through the include_circuit
                // method
                included[i] = false;
            }
            if let Val::PartialPolyApply(_, [fun, arg], ..) = val {
                included[*fun] = true;
                if !self.vals[*fun].next_arg_uses().is_empty() {
                    included[*arg] = true;
                }
            } else {
                for dep in val.ref_indices() {
                    included[*dep] = true;
                }
            }
        }
        let mut gates = Vec::new();
        let mut gates_index = Vec::new();
        for (i, reach) in included.iter().enumerate() {
            if !reach {
                gates_index.push(usize::MAX);
                continue;
            }
            let val = self.vals[i].clone();
            match val {
                Val::Arg(0, arg) => {
                    gates_index.push(gates.len());
                    match arg.checked_sub(arg_depth) {
                        Some(a) => gates.push(PolyGate::Arg(a)),
                        None => gates.push(PolyGate::CapturedArg(arg)),
                    }
                }
                Val::Const(0, c, _) => {
                    gates_index.push(gates.len());
                    gates.push(PolyGate::Const(c))
                }
                Val::PolyOp(op) => {
                    gates_index.push(gates.len());
                    let op = op.remap_indices(&gates_index);
                    gates.push(PolyGate::Op(op))
                }
                Val::Poly(0, circuit, _) | Val::PartialPolyApply(0, _, circuit, _) => {
                    let parameters = self.poly_args(i);
                    let circuit = self.env.lookup_circuit(circuit);
                    let last = self.include_circuit(
                        &circuit,
                        &mut gates,
                        &gates_index,
                        &parameters,
                        arg_depth,
                    );
                    gates_index.push(last);
                }
                _ => {
                    gates_index.push(usize::MAX);
                    continue;
                }
            }
        }
        let (_, arg_deps) = self.deps[root].len().split_at(rank as usize, rank as usize);
        Some(PolyCircuit { gates, arg_deps })
    }

    pub fn vals(self) -> Vals<Γ::PolyCircuitId> {
        Vals {
            vals: self.vals,
            deps: self.deps,
        }
    }

    pub fn quoted_val(&mut self, root: usize) -> Val<Γ::PolyCircuitId> {
        match &self.vals[root] {
            Val::Arg(_, _) | Val::PolyOp(_) | Val::PartialPolyApply(0, ..) => {
                let arg_count = self.vals[root].rank().unwrap();
                let circuit = self.create_polycircuit(root, arg_count, 0).unwrap();
                let circuit_id = self.env.intern_circuit(Arc::new(circuit));
                Val::Poly(arg_count, circuit_id, self.vals[root].deps())
            }
            otherwise => otherwise.clone(),
        }
    }
}
