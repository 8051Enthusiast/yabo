#![allow(mixed_script_confusables)]
pub mod prob_array;
pub mod regex;
mod represent;

pub use represent::len_graph;

use prob_array::{RandomArray, UNINIT};
use smallvec::SmallVec;
use std::{
    cmp::Ordering,
    collections::HashMap,
    hash::{BuildHasher, Hash},
    ops::Index,
    slice,
    sync::Arc,
};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct SmallBitVec(SmallVec<[u64; 2]>);

impl Index<usize> for SmallBitVec {
    type Output = bool;

    fn index(&self, index: usize) -> &Self::Output {
        let (word, bit) = (index / 64, index % 64);
        if (self.0[word] >> bit) & 1 != 0 {
            &true
        } else {
            &false
        }
    }
}

impl SmallBitVec {
    pub fn new(bits: &[bool]) -> Self {
        Self(
            bits.chunks(64)
                .map(|word| word.iter().enumerate().map(|(i, b)| (*b as u64) << i).sum())
                .collect(),
        )
    }

    pub fn ones(bits: usize) -> Self {
        let mut words = SmallVec::with_capacity((bits + 63) / 64);
        for _ in 0..words.capacity() {
            words.push(u64::MAX);
        }
        SmallBitVec(words)
    }

    pub fn zeroes(bits: usize) -> Self {
        let mut words = SmallVec::with_capacity((bits + 63) / 64);
        for _ in 0..words.capacity() {
            words.push(0);
        }
        SmallBitVec(words)
    }

    pub fn or(&self, other: &Self) -> Self {
        let mut words = SmallVec::with_capacity(self.0.len().min(other.0.len()));
        for (a, b) in self.0.iter().zip(other.0.iter()) {
            words.push(a | b);
        }
        SmallBitVec(words)
    }

    pub fn is_zero_until(&self, idx: usize) -> bool {
        (0..idx).all(|i| !self[i])
    }

    pub fn set(&mut self, idx: usize) {
        let (word, bit) = (idx / 64, idx % 64);
        self.0[word] |= 1 << bit;
    }
}

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
    Op(PolyOp),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct PolyCircuit {
    pub gates: Vec<PolyGate>,
    pub deps: SmallBitVec,
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
    Copy(usize),
}

impl<ParserRef> Term<ParserRef> {
    fn ref_indices(&self) -> &[usize] {
        match self {
            Term::Pd(_) | Term::Arg(_) | Term::Const(_) | Term::Opaque | Term::Arr => &[],
            Term::OpaqueUn(x) | Term::Neg(x) | Term::Copy(x) => slice::from_ref(x),
            Term::OpaqueBin(x)
            | Term::Apply(x)
            | Term::Unify(x)
            | Term::UnifyDyn(x)
            | Term::Mul(x)
            | Term::Add(x) => x,
        }
    }
}

#[derive(Default, PartialEq, Eq, Debug, Clone)]
pub struct SizeExpr<Pd> {
    pub terms: Vec<Term<Pd>>,
}

impl<Pd> SizeExpr<Pd> {
    pub fn new() -> Self {
        Self { terms: vec![] }
    }

    pub fn push(&mut self, term: Term<Pd>) -> usize {
        self.terms.push(term);
        self.terms.len() - 1
    }
}

pub enum Fun<ParserRef> {
    ParserRef(ParserRef),
    Static(SmallBitVec),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Val<PolyCircuitId> {
    Undefined,
    Const(u32, i128),
    Arg(u32, u32),
    PolyOp(PolyOp),
    Poly(u32, PolyCircuitId, SmallBitVec),
    PartialPolyApply(u32, [usize; 2], PolyCircuitId, SmallBitVec),
    Static(u32, SmallBitVec),
    Dynamic,
}

impl<PolyCircuitId: Clone + std::fmt::Debug> Val<PolyCircuitId> {
    pub fn is_dynamic(&self) -> bool {
        matches!(self, Val::Dynamic)
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
            Val::Dynamic => None,
        }
    }

    fn as_arg_fun(&self) -> Self {
        match self {
            Val::Static(rank, deps) if !deps.is_zero_until(*rank as usize) => Val::Dynamic,
            Val::Poly(rank, .., x) | Val::PartialPolyApply(rank, .., x)
                if !x.is_zero_until(*rank as usize) =>
            {
                Val::Dynamic
            }
            otherwise => otherwise.clone(),
        }
    }

    pub fn uses_next_arg(&self) -> bool {
        match self {
            Val::Undefined | Val::Const(_, _) | Val::Arg(_, _) | Val::PolyOp(_) => false,
            Val::Poly(rank, _, deps)
            | Val::PartialPolyApply(rank, _, _, deps)
            | Val::Static(rank, deps) => deps[*rank as usize - 1],
            Val::Dynamic => true,
        }
    }

    fn deps(&self) -> SmallBitVec {
        match self {
            Val::Const(rank, _) | Val::Arg(rank, _) => SmallBitVec::zeroes(*rank as usize),
            Val::Poly(.., x) | Val::PartialPolyApply(.., x) => x.clone(),
            Val::Static(_, deps) => deps.clone(),
            Val::PolyOp(_) | Val::Undefined | Val::Dynamic => SmallBitVec::default(),
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
            Val::Dynamic => 4,
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

#[derive(Clone, Copy)]
pub enum ArgKind {
    Const(u32),
    Dynamic,
}
pub trait Env: Sized {
    type ParserRef: Copy + Eq + std::fmt::Debug;
    type PolyCircuitId: Copy + Eq + std::fmt::Debug;

    fn size_info(&self, pd: &Self::ParserRef) -> Val<Self::PolyCircuitId>;

    fn lookup_circuit(&self, id: Self::PolyCircuitId) -> Arc<PolyCircuit>;
    fn intern_circuit(&self, circuit: Arc<PolyCircuit>) -> Self::PolyCircuitId;
}

pub struct SizeCalcCtx<'a, Γ: Env> {
    env: &'a Γ,
    size_expr: &'a SizeExpr<Γ::ParserRef>,
    poly_vals: Vec<RandomArray>,
    pub vals: Vec<Val<Γ::PolyCircuitId>>,
    arr_circuit: Γ::PolyCircuitId,
    args: &'a [ArgKind],
    arg_poly_vals: Vec<RandomArray>,
}

impl<'a, Γ: Env> SizeCalcCtx<'a, Γ> {
    pub fn new(env: &'a Γ, size_expr: &'a SizeExpr<Γ::ParserRef>, args: &'a [ArgKind]) -> Self {
        // 5 extra slots to avoid reallocation when comparing polynomials
        let mut arg_poly_vals = vec![UNINIT; args.len() + 5];
        for arg in arg_poly_vals.iter_mut() {
            arg.random_element();
        }
        let arr_circuit = PolyCircuit {
            deps: SmallBitVec::ones(2),
            gates: vec![
                PolyGate::Arg(0),
                PolyGate::Arg(1),
                PolyGate::Op(PolyOp::Mul([0, 1])),
            ],
        };
        let arr_circuit = env.intern_circuit(Arc::new(arr_circuit));
        let mut ret = Self {
            env,
            poly_vals: vec![UNINIT; size_expr.terms.len()],
            vals: Vec::with_capacity(size_expr.terms.len()),
            size_expr,
            arr_circuit,
            args,
            arg_poly_vals,
        };
        ret.eval();
        ret
    }

    fn poly_args_impl(&mut self, fun: Val<Γ::PolyCircuitId>) -> Vec<Parameter> {
        match fun {
            Val::PartialPolyApply(_, [fun, arg], ..) => {
                let mut args = self.poly_args_impl(self.vals[fun].clone());
                args.push(Parameter::Val(arg));
                args
            }
            Val::Poly(rank, ..) => Vec::with_capacity(rank as usize),
            _ => panic!("not a polynomial"),
        }
    }

    fn poly_args(&mut self, fun: Val<Γ::PolyCircuitId>) -> Vec<Parameter> {
        let mut args = self.poly_args_impl(fun.clone());
        let rank = match fun {
            Val::PartialPolyApply(rank, ..) => rank,
            Val::Poly(rank, ..) => rank,
            _ => panic!("not a polynomial"),
        };
        // for comparing polynomials, we need to make sure there are at least
        // rank + arglen slots in the arg_poly_vals vector
        let new_size = self
            .arg_poly_vals
            .len()
            .max(rank as usize + self.args.len());
        self.arg_poly_vals.resize_with(new_size, || {
            let mut arr = UNINIT;
            arr.random_element();
            arr
        });
        for i in self.args.len()..(self.args.len() + rank as usize) {
            args.push(Parameter::Arg(i));
        }
        args
    }

    fn eval_circuit(&mut self, circuit: &PolyCircuit, params: &[Parameter]) -> RandomArray {
        let mut poly_vals = vec![UNINIT; circuit.gates.len()];
        for (idx, gate) in circuit.gates.iter().enumerate() {
            let val = &mut poly_vals[idx];
            match gate {
                PolyGate::Const(c) => val.array_from_const(*c),
                PolyGate::Arg(a) => match params[*a as usize] {
                    Parameter::Arg(a) => val.clone_from(&self.arg_poly_vals[a]),
                    Parameter::Val(v) => {
                        self.eval_poly(v);
                        val.clone_from(&self.poly_vals[v])
                    }
                },
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
        let [.., res] = poly_vals[..] else { panic!("empty circuit") };
        res
    }

    fn eval_poly(&mut self, idx: usize) {
        if !self.poly_vals[idx].is_uninit() {
            return;
        }
        match self.vals[idx].clone() {
            Val::Const(_, c) => self.poly_vals[idx].array_from_const(c),
            Val::Arg(_, a) => self.poly_vals[idx].clone_from(&self.arg_poly_vals[a as usize]),
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
                let args = self.poly_args(self.vals[idx].clone());
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
        if deps
            .iter()
            .any(|x| matches!(&self.vals[*x], Val::Undefined))
        {
            return Val::Undefined;
        }
        if deps.iter().any(|x| matches!(&self.vals[*x], Val::Dynamic)) {
            return Val::Dynamic;
        }
        if deps
            .iter()
            .any(|x| matches!(&self.vals[*x], Val::Static(0, ..)))
        {
            return Val::Static(0, SmallBitVec::default());
        }
        let Some(consts) = deps.iter().map(|x| match &self.vals[*x] {
            Val::Const(0, x) => Some(*x),
            _ => None,
        }).collect::<Option<SmallVec<[i128; N]>>>() else {
            return Val::PolyOp(poly_op);
        };
        let (res, overflow) = exec_op(consts.as_slice().try_into().unwrap());
        if overflow {
            Val::Undefined
        } else {
            Val::Const(0, res)
        }
    }

    fn opaque_op(&self, args: &[usize]) -> Val<Γ::PolyCircuitId> {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        enum State {
            Static,
            Dynamic,
            Undefined,
        }

        let mut state = State::Static;
        for arg in args {
            match &self.vals[*arg] {
                Val::Undefined => {
                    state = state.max(State::Undefined);
                }
                Val::Dynamic => {
                    state = state.max(State::Dynamic);
                }
                _ => {}
            }
        }
        match state {
            State::Static => Val::Static(0, SmallBitVec::default()),
            State::Dynamic => Val::Dynamic,
            State::Undefined => Val::Undefined,
        }
    }

    fn apply_fun(&self, fun: usize, arg: usize) -> Val<Γ::PolyCircuitId> {
        let argv = self.vals[arg].as_arg_fun();
        match (&self.vals[fun], &argv, self.vals[fun].uses_next_arg()) {
            (Val::Undefined, ..) | (_, Val::Undefined, true) => Val::Undefined,
            (Val::Arg(rank @ 1.., a), _, _) => Val::Arg(*rank - 1, *a),
            (Val::Const(rank @ 1.., c), _, _) => Val::Const(*rank - 1, *c),
            (Val::Static(1.., _), Val::Dynamic, true) => Val::Dynamic,
            (Val::Static(rank @ 1.., deps), ..) => {
                if *rank == 1 {
                    Val::Static(*rank - 1, SmallBitVec::default())
                } else {
                    Val::Static(*rank - 1, deps.clone())
                }
            }
            (
                Val::PartialPolyApply(rank @ 1.., _, circ, deps)
                | Val::Poly(rank @ 1.., circ, deps),
                argv,
                true,
            ) => {
                if argv.is_dynamic() {
                    Val::Dynamic
                } else if argv.is_poly() {
                    Val::PartialPolyApply(*rank - 1, [fun, arg], *circ, deps.clone())
                } else {
                    Val::Static(*rank - 1, deps.clone())
                }
            }
            (
                Val::PartialPolyApply(rank @ 1.., _, circ, deps)
                | Val::Poly(rank @ 1.., circ, deps),
                _,
                false,
            ) => Val::PartialPolyApply(*rank - 1, [fun, arg], *circ, deps.clone()),

            _ => Val::Dynamic,
        }
    }

    fn unify(&mut self, ops @ [lidx, ridx]: [usize; 2], dy: bool) -> Val<Γ::PolyCircuitId> {
        let maybe_static = |rank, deps| {
            if dy {
                Val::Dynamic
            } else {
                Val::Static(rank, deps)
            }
        };
        match ops.map(|x| self.vals[x].clone()) {
            [Val::Undefined, other] | [other, Val::Undefined] => other,
            [Val::Dynamic, _] | [_, Val::Dynamic] => Val::Dynamic,
            [lhs, rhs] if lhs.rank() != rhs.rank() => Val::Dynamic,
            [Val::Static(rank, ldeps), Val::Static(_, rdeps)] => {
                let deps = ldeps.or(&rdeps);
                maybe_static(rank, deps)
            }
            [Val::Static(rank, ldeps), Val::Poly(.., x) | Val::PartialPolyApply(.., x)]
            | [Val::Poly(.., x) | Val::PartialPolyApply(.., x), Val::Static(rank, ldeps)] => {
                let deps = ldeps.or(&x);
                maybe_static(rank, deps)
            }
            [Val::Static(rank, deps), Val::Arg(..) | Val::Const(..) | Val::PolyOp(..)]
            | [Val::PolyOp(..) | Val::Arg(..) | Val::Const(..), Val::Static(rank, deps)] => {
                maybe_static(rank, deps)
            }
            [Val::Const(rank, a), Val::Const(_, b)] => {
                if a == b {
                    Val::Const(rank, a)
                } else {
                    maybe_static(rank, SmallBitVec::zeroes(rank as usize))
                }
            }
            [Val::Arg(rank, a), Val::Arg(_, b)] => {
                if a == b {
                    Val::Arg(rank, a)
                } else {
                    maybe_static(rank, SmallBitVec::zeroes(rank as usize))
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
                ops.map(|x| self.eval_poly(x));
                if self.poly_vals[lidx] == self.poly_vals[ridx] {
                    lhs
                } else {
                    let rank = lhs.rank().unwrap();
                    let deps = lhs.deps();
                    maybe_static(rank, deps)
                }
            }
        }
    }

    fn eval(&mut self) {
        for term_idx in 0..self.size_expr.terms.len() {
            let term = self.size_expr.terms[term_idx];
            let val = match &term {
                Term::Pd(pd) => self.env.size_info(pd),
                Term::Arg(a) => match self.args[*a as usize] {
                    ArgKind::Const(rank) => Val::Arg(rank, *a),
                    ArgKind::Dynamic => Val::Dynamic,
                },
                Term::Apply([fun, arg]) => self.apply_fun(*fun, *arg),
                Term::Const(c) => Val::Const(0, *c),
                Term::Opaque => Val::Dynamic,
                Term::OpaqueUn(arg) => self.opaque_op(&[*arg]),
                Term::OpaqueBin([lhs, rhs]) => self.opaque_op(&[*lhs, *rhs]),
                Term::Mul(ops) => self.poly_op(
                    *ops,
                    |[lhs, rhs]| lhs.overflowing_mul(rhs),
                    PolyOp::Mul(*ops),
                ),
                Term::Add(ops) => self.poly_op(
                    *ops,
                    |[lhs, rhs]| lhs.overflowing_add(rhs),
                    PolyOp::Add(*ops),
                ),
                Term::Neg(arg) => {
                    self.poly_op([*arg], |[x]| x.overflowing_neg(), PolyOp::Neg(*arg))
                }
                Term::Arr => Val::Poly(2, self.arr_circuit, SmallBitVec::ones(2)),
                Term::Unify(ops) => self.unify(*ops, false),
                Term::UnifyDyn(ops) => self.unify(*ops, true),
                Term::Copy(val) => self.vals[*val].clone(),
            };
            self.vals.push(val);
        }
    }

    pub fn static_arg_deps(&mut self) -> Vec<SmallBitVec> {
        let mut arg_deps = vec![SmallBitVec::zeroes(self.args.len()); self.size_expr.terms.len()];
        for (i, term) in self.size_expr.terms.iter().enumerate() {
            for dep in term.ref_indices() {
                arg_deps[i] = arg_deps[i].or(&arg_deps[*dep]);
            }
            if let Term::Arg(a) = term {
                arg_deps[i].set(self.args.len() - *a as usize - 1);
            }
        }
        arg_deps
    }

    fn include_circuit(
        &self,
        included_circuit: &PolyCircuit,
        new_gates: &mut Vec<PolyGate>,
        term_indices: &[usize],
        args: &[Parameter],
    ) -> usize {
        let mut index = Vec::new();
        for gate in included_circuit.gates.iter() {
            match gate {
                PolyGate::Const(c) => {
                    index.push(new_gates.len());
                    new_gates.push(PolyGate::Const(*c));
                }
                PolyGate::Arg(a) => match args[*a as usize] {
                    Parameter::Arg(a) => {
                        index.push(new_gates.len());
                        new_gates.push(PolyGate::Arg(a as u32));
                    }
                    Parameter::Val(v) => {
                        index.push(term_indices[v]);
                    }
                },
                PolyGate::Op(op) => {
                    index.push(new_gates.len());
                    let remapped = op.remap_indices(&index);
                    new_gates.push(PolyGate::Op(remapped));
                }
            }
        }
        *index.last().unwrap()
    }

    fn create_polycircuit(&mut self, root: usize) -> Option<PolyCircuit> {
        if !self.vals.get(root).map_or(false, |x| x.is_poly()) {
            return None;
        }
        let mut included = vec![false; self.vals.len()];
        let mut args = SmallBitVec::zeroes(self.args.len());
        included[root] = true;
        for (i, val) in self.vals.iter().enumerate().rev() {
            if !included[i] {
                continue;
            }
            if let Val::Arg(_, a) = val {
                args.set(self.args.len() - *a as usize - 1);
            }
            if val.is_poly_fun() {
                // higher order functions are not directly included
                // in the circuit but flattened through the include_circuit
                // method
                included[i] = false;
            }
            for dep in val.ref_indices() {
                included[*dep] = true;
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
                Val::Arg(0, a) => {
                    gates_index.push(gates.len());
                    gates.push(PolyGate::Arg(a))
                }
                Val::Const(0, c) => {
                    gates_index.push(gates.len());
                    gates.push(PolyGate::Const(c))
                }
                Val::PolyOp(op) => {
                    gates_index.push(gates.len());
                    let op = op.remap_indices(&gates_index);
                    gates.push(PolyGate::Op(op))
                }
                Val::Poly(0, circuit, _) | Val::PartialPolyApply(0, _, circuit, _) => {
                    let parameters = self.poly_args(self.vals[i].clone());
                    let circuit = self.env.lookup_circuit(circuit);
                    let last =
                        self.include_circuit(&circuit, &mut gates, &gates_index, &parameters);
                    gates_index.push(last);
                }
                _ => {
                    gates_index.push(usize::MAX);
                    continue;
                }
            }
        }
        Some(PolyCircuit { gates, deps: args })
    }

    pub fn call_site_deps<T: Copy + Eq + Hash, H: BuildHasher + Default>(
        &self,
        origins: &[T],
        sizes: &[usize],
        deps: &mut HashMap<T, SmallBitVec, H>,
    ) {
        let mut idx = sizes.len();
        while idx > 0 {
            idx -= 1;
            let size@1.. = sizes[idx] else {
                continue;
            };
            let mut call_deps = SmallBitVec::zeroes(size);
            let mut f_idx = idx;
            for i in (0..size).rev() {
                let Term::Apply([f, _]) = &self.size_expr.terms[f_idx] else {
                    panic!("call site does not consist of apply terms")
                };
                if self.vals[*f].uses_next_arg() {
                    call_deps.set(i);
                }
                f_idx = *f;
            }
            let origin = origins[idx];
            deps.insert(origin, call_deps);
        }
    }

    pub fn vals(self) -> Vec<Val<Γ::PolyCircuitId>> {
        self.vals
    }

    pub fn fun_val(&mut self, root: usize) -> Val<Γ::PolyCircuitId> {
        let arg_count = self.args.len() as u32;
        match &self.vals[root] {
            Val::Undefined => Val::Undefined,
            Val::Const(0, c) => Val::Const(arg_count, *c),
            Val::Arg(0, _) | Val::PolyOp(_) | Val::Poly(0, ..) | Val::PartialPolyApply(0, ..) => {
                let circuit = self.create_polycircuit(root).unwrap();
                let deps = circuit.deps.clone();
                let circuit_id = self.env.intern_circuit(Arc::new(circuit));
                Val::Poly(arg_count, circuit_id, deps)
            }
            Val::Static(0, _) => {
                let deps = self.static_arg_deps().remove(root);
                Val::Static(arg_count, deps)
            }
            Val::Dynamic => Val::Dynamic,
            _ => panic!("cannot have higher-order function as total length"),
        }
    }
}
