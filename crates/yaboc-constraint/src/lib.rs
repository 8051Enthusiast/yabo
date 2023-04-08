mod len_term;

use fxhash::FxHashMap;
use yaboc_base::{
    error::SResult,
    interner::{Interner, Regex},
};
use yaboc_dependents::Dependents;
use yaboc_hir as hir;
use yaboc_len::{regex::RegexError, ArgKind, Env, SizeCalcCtx, Val};
use yaboc_resolve::{parserdef_ssc::FunctionSscId, Resolves};

use len_term::{len_term, PdLenTerm};
use yaboc_types::{PrimitiveType, Type};

#[salsa::query_group(ConstraintDatabase)]
pub trait Constraints: Interner + Resolves + Dependents {
    fn regex_len(&self, regex: Regex) -> Result<Option<i128>, RegexError>;
    fn len_term(&self, pd: hir::ParserDefId) -> SResult<PdLenTerm>;
    fn fun_len(&self, pd: hir::ParserDefId) -> Val;
    fn ssc_fun_len(&self, ssc: FunctionSscId) -> Vec<Val>;
}

pub fn regex_len(db: &dyn Constraints, regex: Regex) -> Result<Option<i128>, RegexError> {
    let regex_str = db.lookup_intern_regex(regex);
    yaboc_len::regex::regex_len(&regex_str)
}

pub struct LenInferCtx<'a, DB: Constraints + ?Sized> {
    db: &'a DB,
    local_vals: FxHashMap<hir::ParserDefId, Val>,
    local_terms: &'a FxHashMap<hir::ParserDefId, Option<PdLenTerm>>,
}

impl<'a, DB: Constraints + ?Sized> LenInferCtx<'a, DB> {
    pub fn new(
        db: &'a DB,
        local_vals: FxHashMap<hir::ParserDefId, Val>,
        local_terms: &'a FxHashMap<hir::ParserDefId, Option<PdLenTerm>>,
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
                let Some(term) = &term else {continue};
                let args = arg_kinds(self.db, *pd).unwrap_or_default();
                let mut subctx = SizeCalcCtx::new(&*self, &*term.expr, &args);
                let new_val = subctx.fun_val(term.root);
                if new_val.has_expanded_from(&self.local_vals[pd]) {
                    changed = true;
                    self.local_vals.insert(*pd, new_val);
                }
            }
        }
    }
}

impl<'db, DB: Constraints + ?Sized> Env for LenInferCtx<'db, DB> {
    type ParserRef = hir::ParserDefId;

    fn size_info(&self, pd: &Self::ParserRef) -> Val {
        if let Some(val) = self.local_vals.get(pd).cloned() {
            val
        } else {
            self.db.fun_len(*pd)
        }
    }
}

fn arg_kinds(db: &(impl Constraints + ?Sized), pd: hir::ParserDefId) -> SResult<Vec<ArgKind>> {
    let signature = db.parser_args(pd)?;
    let Some(args) = &signature.args else {
        return Ok(Vec::new())
    };

    let mut ret = Vec::new();
    for arg in args.iter() {
        let (rank, inner_ty) = if let Type::FunctionArg(inner, args) = db.lookup_intern_type(*arg) {
            (args.len() as u32, inner)
        } else {
            (0, *arg)
        };
        let is_lenable = matches!(
            db.lookup_intern_type(inner_ty),
            Type::ParserArg { .. }
                | Type::Primitive(PrimitiveType::Int | PrimitiveType::Bit | PrimitiveType::Char)
        );
        let arg_kind = if is_lenable {
            ArgKind::Const(rank)
        } else {
            ArgKind::Dynamic
        };
        ret.push(arg_kind);
    }
    Ok(ret)
}

pub fn ssc_fun_len(db: &dyn Constraints, ssc: FunctionSscId) -> Vec<Val> {
    let pds = db.lookup_intern_recursion_scc(ssc);
    let terms = pds.iter().map(|x| (*x, db.len_term(*x).ok())).collect();

    let vals = pds
        .iter()
        .map(|x| (*x, yaboc_len::Val::Undefined))
        .collect();

    let mut ctx = LenInferCtx::new(db, vals, &terms);

    ctx.infer();

    pds.iter()
        .map(|x| ctx.local_vals.remove(x).unwrap())
        .collect()
}

pub fn fun_len(db: &dyn Constraints, pd: hir::ParserDefId) -> Val {
    let Ok(ssc) = db.parser_ssc(pd) else {
        return Val::Undefined
    };
    let pds = db.lookup_intern_recursion_scc(ssc);
    let idx = pds.iter().position(|x| *x == pd).unwrap();
    let fun_vals = db.ssc_fun_len(ssc);
    fun_vals[idx].clone()
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
        assert_eq!(ctx.db.fun_len(one), Val::Const(0, 1));
        assert_eq!(ctx.db.fun_len(two), Val::Const(0, 2));
        assert_eq!(ctx.db.fun_len(three), Val::Const(0, 3));
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
        assert_eq!(ctx.db.fun_len(const_size_choice), Val::Const(0, 3));
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
        assert_eq!(ctx.db.fun_len(rec), Val::Const(0, 5));
        assert_eq!(ctx.db.fun_len(rec2), Val::Dynamic);
    }

    #[test]
    fn poly_size() {
        let ctx = Context::<ConstraintTestDatabase>::mock(
            r#"
            fun for['a] *> compose(a: for['a] *> for['b], b: for['b] *> 'c) = {
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
            def *square(n: int) = [~](n * n)
            def *poly_unify(n: int, m: int) = {
                | square(n + m)
                  poly_unify(0, 0)
                | [~](2 * n * m - 1)
                  [~](n * n)
                  [~](m * m)
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
            def *stat(a: int, b: int) = [~](1 << a) |> [~](1 << b)
        "#,
        );
        let stat = ctx.parser("stat");
        let Val::Static(2, deps) = ctx.db.fun_len(stat) else {
            panic!("got unexpected {:?}", stat)
        };
        assert!(deps[0]);
        // currently not as fine-grained because that's a pain
        // assert!(!deps[1]);
    }
}
