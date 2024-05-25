pub mod error;
mod len;
pub mod represent;

use std::sync::Arc;

use fxhash::FxHashMap;
use hir::HirIdWrapper;
use salsa::InternKey;
use yaboc_base::{
    error::{SResult, Silencable, SilencedError},
    interner::{Interner, Regex},
    source::IndirectSpan,
};
use yaboc_dependents::Dependents;
use yaboc_hir as hir;
use yaboc_len::{depvec::ArgDeps, ArgRank, Env, PolyCircuit, SizeCalcCtx, Term, Val};
use yaboc_resolve::{parserdef_ssc::FunctionSscId, Resolves};

use len::len_term;
pub use len::{Origin, PdLenTerm};
use yaboc_types::{Type, TypeId};

#[salsa::query_group(ConstraintDatabase)]
pub trait Constraints: Interner + Resolves + Dependents {
    fn regex_len(&self, regex: Regex) -> SResult<Option<i128>>;
    fn len_term(&self, pd: hir::ParserDefId) -> SResult<Arc<PdLenTerm>>;
    fn fun_len(&self, pd: hir::ParserDefId) -> LenVal;
    fn len_vals(&self, pd: hir::ParserDefId) -> Arc<LenVals>;
    fn ssc_len_vals(&self, ssc: FunctionSscId) -> Arc<Vec<LenVals>>;
    fn len_errors(&self, pd: hir::ParserDefId) -> SResult<Vec<LenError>>;

    #[salsa::interned]
    fn intern_polycircuit(&self, circuit: Arc<PolyCircuit>) -> PolyCircuitId;
}

pub type LenVal = Val<PolyCircuitId>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct LenVals {
    pub vals: Vec<LenVal>,
    pub deps: Vec<ArgDeps>,
    pub fun_val: LenVal,
    pub root: usize,
}

impl Default for LenVals {
    fn default() -> Self {
        Self {
            vals: Default::default(),
            deps: Default::default(),
            fun_val: Val::Undefined,
            root: Default::default(),
        }
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub struct PolyCircuitId(salsa::InternId);

impl InternKey for PolyCircuitId {
    fn from_intern_id(v: salsa::InternId) -> Self {
        PolyCircuitId(v)
    }

    fn as_intern_id(&self) -> salsa::InternId {
        self.0
    }
}

pub fn regex_len(db: &dyn Constraints, regex: Regex) -> SResult<Option<i128>> {
    let Ok(regex) = db.resolve_regex(regex) else {
        return Err(SilencedError::new());
    };
    Ok(yaboc_len::regex::regex_len(&regex))
}

pub struct LenInferCtx<'a, DB: Constraints + ?Sized> {
    db: &'a DB,
    local_vals: FxHashMap<hir::ParserDefId, LenVals>,
    local_terms: &'a FxHashMap<hir::ParserDefId, Option<Arc<PdLenTerm>>>,
}

impl<'a, DB: Constraints + ?Sized> LenInferCtx<'a, DB> {
    pub fn new(
        db: &'a DB,
        local_vals: FxHashMap<hir::ParserDefId, LenVals>,
        local_terms: &'a FxHashMap<hir::ParserDefId, Option<Arc<PdLenTerm>>>,
    ) -> Self {
        Self {
            db,
            local_vals,
            local_terms,
        }
    }

    pub fn infer(&mut self) {
        let mut changed = true;
        while changed {
            changed = false;
            for (pd, term) in self.local_terms.iter() {
                let Some(term) = &term else { continue };
                let args = arg_ranks(self.db, *pd).unwrap_or_default();
                let mut subctx = SizeCalcCtx::new(&*self, &term.expr, &args);
                let new_val = subctx.fun_val(term.root, term.is_val_fun);
                let old_val = self
                    .local_vals
                    .get(pd)
                    .map_or(Val::Unsized, |x| x.fun_val.clone());
                changed |= new_val.has_expanded_from(&old_val);

                let root = term.root;
                let vals = subctx.vals();
                let new_term_vals = LenVals {
                    vals: vals.vals,
                    deps: vals.deps,
                    fun_val: new_val,
                    root,
                };
                self.local_vals.insert(*pd, new_term_vals);
            }
        }
    }
}

impl<'db, DB: Constraints + ?Sized> Env for LenInferCtx<'db, DB> {
    type ParserRef = hir::ParserDefId;
    type PolyCircuitId = PolyCircuitId;

    fn size_info(&self, pd: &Self::ParserRef) -> LenVal {
        if let Some(val) = self.local_vals.get(pd) {
            val.fun_val.clone()
        } else {
            self.db.fun_len(*pd)
        }
    }

    fn lookup_circuit(&self, id: Self::PolyCircuitId) -> Arc<PolyCircuit> {
        self.db.lookup_intern_polycircuit(id)
    }

    fn intern_circuit(&self, circuit: Arc<PolyCircuit>) -> Self::PolyCircuitId {
        self.db.intern_polycircuit(circuit)
    }
}
fn arg_rank(db: &(impl Constraints + ?Sized), ty: TypeId) -> SResult<ArgRank> {
    Ok(ArgRank(
        match db.lookup_intern_type(db.least_deref_type(ty)?) {
            Type::FunctionArg(_, args) => args.len() as u32,
            _ => 0,
        },
    ))
}

fn arg_ranks(db: &(impl Constraints + ?Sized), pd: hir::ParserDefId) -> SResult<Vec<ArgRank>> {
    let signature = db.parser_args(pd)?;
    let Some(args) = &signature.args else {
        return Ok(Vec::new());
    };
    args.iter().map(|arg_ty| arg_rank(db, *arg_ty)).collect()
}

pub fn ssc_len_vals(db: &dyn Constraints, ssc: FunctionSscId) -> Arc<Vec<LenVals>> {
    let pds = db.lookup_intern_recursion_scc(ssc);
    let terms = pds.iter().map(|x| (*x, db.len_term(*x).ok())).collect();

    let vals = pds.iter().map(|x| (*x, LenVals::default())).collect();

    let mut ctx = LenInferCtx::new(db, vals, &terms);

    ctx.infer();

    Arc::new(
        pds.iter()
            .map(|x| ctx.local_vals.remove(x).unwrap())
            .collect(),
    )
}

pub fn fun_len(db: &dyn Constraints, pd: hir::ParserDefId) -> LenVal {
    let Ok(ssc) = db.parser_ssc(pd) else {
        return Val::Undefined;
    };
    let pds = db.lookup_intern_recursion_scc(ssc);
    let idx = pds.iter().position(|x| *x == pd).unwrap();
    let fun_vals = db.ssc_len_vals(ssc);
    fun_vals[idx].fun_val.clone()
}

pub fn len_vals(db: &dyn Constraints, pd: hir::ParserDefId) -> Arc<LenVals> {
    let Ok(ssc) = db.parser_ssc(pd) else {
        return Default::default();
    };
    let pds = db.lookup_intern_recursion_scc(ssc);
    let idx = pds.iter().position(|x| *x == pd).unwrap();
    let fun_vals = db.ssc_len_vals(ssc);
    Arc::new(fun_vals[idx].clone())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LenError {
    NonsizedInArray(IndirectSpan),
    SizedWithNonsizedArg {
        loc: IndirectSpan,
        arg_loc: IndirectSpan,
    },
    Silenced(SilencedError),
}

impl Silencable for LenError {
    type Out = SilencedError;

    fn silence(self) -> Self::Out {
        match self {
            LenError::Silenced(e) => e,
            LenError::NonsizedInArray(_) | LenError::SizedWithNonsizedArg { .. } => {
                SilencedError::new()
            }
        }
    }
}

impl From<SilencedError> for LenError {
    fn from(e: SilencedError) -> Self {
        LenError::Silenced(e)
    }
}

pub fn len_errors(db: &dyn Constraints, pd: hir::ParserDefId) -> SResult<Vec<LenError>> {
    let parserdef = pd.lookup(db)?;
    let terms = db.len_term(pd)?;
    let lens = db.len_vals(pd);
    let num_args = db.argnum(pd)?.unwrap_or(0);
    let mut errs = Vec::new();
    let get_span = |idx| -> SResult<_> {
        match terms.term_spans[idx] {
            Origin::Expr(expr, idx) => {
                let span_idx = db.resolve_expr(expr)?.data[idx];
                Ok(IndirectSpan::new(expr.0, span_idx))
            }
            Origin::Node(id) => Ok(IndirectSpan::default_span(id)),
        }
    };
    let arg_deps = &lens.deps;
    for term in terms.expr.terms.iter() {
        let arg = if let Term::Apply([arr, arg]) = term {
            if terms.expr.terms[*arr] != Term::Arr {
                continue;
            }
            arg
        } else if let Term::Size(true, arg) = term {
            arg
        } else {
            continue;
        };
        if lens.vals[*arg].is_unsized() {
            errs.push(LenError::NonsizedInArray(get_span(*arg)?));
            continue;
        }
        for j in 0..num_args {
            if !arg_deps[*arg].0[0].has_len(j) {
                continue;
            }
            let arg_id = parserdef.args.as_ref().unwrap()[j];
            let arg_span = IndirectSpan::default_span(arg_id.0);
            errs.push(LenError::SizedWithNonsizedArg {
                loc: get_span(*arg)?,
                arg_loc: arg_span,
            });
            break;
        }
    }

    Ok(errs)
}

#[cfg(test)]
mod tests {
    use hir::Parser;
    use yaboc_ast::{import::Import, AstDatabase};
    use yaboc_base::{
        config::ConfigDatabase, interner::InternerDatabase, source::FileDatabase, Context,
    };
    use yaboc_dependents::DependentsDatabase;
    use yaboc_hir::HirDatabase;
    use yaboc_hir_types::HirTypesDatabase;
    use yaboc_len::depvec::DepVec;
    use yaboc_resolve::ResolveDatabase;
    use yaboc_types::TypeInternerDatabase;

    use super::*;

    #[salsa::database(
        InternerDatabase,
        ConfigDatabase,
        AstDatabase,
        FileDatabase,
        HirDatabase,
        ResolveDatabase,
        ConstraintDatabase,
        DependentsDatabase,
        TypeInternerDatabase,
        HirTypesDatabase
    )]
    #[derive(Default)]
    pub struct ConstraintTestDatabase {
        storage: salsa::Storage<ConstraintTestDatabase>,
    }

    impl salsa::Database for ConstraintTestDatabase {}

    #[test]
    fn straightline_constant_size() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            def *one = ~
            def *two = { ~,~ }
            def *three = { one, two }
        "#,
        );
        let one = ctx.parser("one");
        let two = ctx.parser("two");
        let three = ctx.parser("three");
        assert_eq!(ctx.db.fun_len(one), Val::Const(0, 1, DepVec::default()));
        assert_eq!(ctx.db.fun_len(two), Val::Const(0, 2, DepVec::default()));
        assert_eq!(ctx.db.fun_len(three), Val::Const(0, 3, DepVec::default()));
    }

    #[test]
    fn same_const_size_choice() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            def *one = ~
            def *const_size_choice = {
                | left: one, one, one
                | right: ~, ~, ~
            }
        "#,
        );
        let const_size_choice = ctx.parser("const_size_choice");
        assert_eq!(
            ctx.db.fun_len(const_size_choice),
            Val::Const(0, 3, DepVec::default())
        );
    }

    #[test]
    fn const_size_rec() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            def *rec = {
                | /AAAAA/?
                | rec
            }
            def *rec2 = {
                | /AAAAA/?
                | ~, rec
            }
        "#,
        );
        let rec = ctx.parser("rec");
        let rec2 = ctx.parser("rec2");
        assert_eq!(ctx.db.fun_len(rec), Val::Const(0, 5, DepVec::default()));
        let Val::Unsized = ctx.db.fun_len(rec2) else {
            panic!("got unexpected {:?}", ctx.db.fun_len(rec2))
        };
    }

    #[test]
    fn poly_size() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            fun ['a] *> compose(a: ['a] *> ['b], b: ['b] *> 'c) = {
              x: a, let return = x *> b
            }
        "#,
        );
        let compose = ctx.parser("compose");
        assert!(matches!(ctx.db.fun_len(compose), Val::Poly(..)));
    }

    #[test]
    fn poly_unify() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            def *square(n: int) = [n * n]
            def *poly_unify(n: int, m: int) = {
                | square(n + m)
                  poly_unify(0, 0)
                | [2 * n * m - 1]
                  [n * n]
                  [m * m]
                  ~
            }
        "#,
        );
        let poly_unify = ctx.parser("poly_unify");
        assert!(matches!(ctx.db.fun_len(poly_unify), Val::Poly(..)));
    }

    #[test]
    fn static_size() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            def *stat(a: int, b: int) = [1 << a] |> [1 << b]
        "#,
        );
        let stat = ctx.parser("stat");
        let len = ctx.db.fun_len(stat);
        let Val::Static(2, deps) = len else {
            panic!("got unexpected {:?}", len)
        };
        // note that the order of the deps is reversed
        assert!(deps.has_val(1));
        assert!(!deps.has_val(0));
    }
    #[test]
    fn dyn_recurse() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            fun *number_acc(base: int, n: int): int = {
              x: ~
              let s = n * base + x
              | return: number_acc?(base, s)
              | let return = s
            }"#,
        );
        let number_acc = ctx.parser("number_acc");
        let len = ctx.db.fun_len(number_acc);
        let Val::Unsized = len else {
            panic!("got unexpected {:?}", len)
        };
    }
    #[test]
    fn backtrack_dep() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            def *backtrack_if(x: int) = x if 0 then [1] else [2]
            def *backtrack_when(x: int) = when?(x == 0) then [1] else [2]
            def *backtrack_field(x: int) = {: let y = x if 0 :}.?y then [1] else [2]
            "#,
        );
        for parser_name in ["backtrack_if", "backtrack_when", "backtrack_field"] {
            let parser = ctx.parser(parser_name);
            let len = ctx.db.fun_len(parser);
            let Val::Static(1, deps) = len else {
                panic!("got unexpected {:?}", len)
            };
            assert!(deps.has_val(0));
        }
    }
    #[test]
    fn val_fun() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            fun uses_vals(x: int): [u8] *> int = x then {
                ~
                let return = x
            }
            fun *test(x: int, y: int) = {
                [y]
                uses_vals(x)
            }
            "#,
        );
        let test = ctx.parser("test");
        let len = ctx.db.fun_len(test);
        let Val::Poly(2, _, deps) = len else {
            panic!("got unexpected {:?}", len)
        };
        assert!(deps.has_val(1));
        assert!(deps.has_val(0));
    }
}
