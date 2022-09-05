pub mod parserdef_ssc;
pub mod refs;

use std::{sync::Arc, collections::BTreeMap};

use parserdef_ssc::{mod_parser_ssc, parser_ssc};

use crate::{error::SResult, hir};

use self::parserdef_ssc::FunctionSscId;

#[salsa::query_group(ResolveDatabase)]
pub trait Resolves: crate::hir::Hirs {
    #[salsa::interned]
    fn intern_recursion_scc(&self, functions: Vec<hir::ParserDefId>) -> FunctionSscId;
    fn mod_parser_ssc(
        &self,
        module: hir::ModuleId,
    ) -> SResult<Arc<BTreeMap<hir::ParserDefId, FunctionSscId>>>;
    fn parser_ssc(&self, parser: hir::ParserDefId) -> SResult<FunctionSscId>;
}

#[cfg(test)]
mod tests {
    use crate::{context::Context, resolve::Resolves};

    #[test]
    fn recursion_ssc() {
        let ctx = Context::mock(
            r#"
def for [u8] *> a = {x: c, y: {b, z: d,},}
def for [u8] *> b = {x: a, y: c,}
def for [u8] *> c = {x: c,}
def for [u8] *> d = {let a: u64 = 1, let b: u64 = a + 1,}
def for [u8] *> e = {}
            "#,
        );
        let a = ctx.parser("a");
        let b = ctx.parser("b");
        let c = ctx.parser("c");
        let d = ctx.parser("d");
        let e = ctx.parser("d");
        let get_ssc = |x| ctx.db.parser_ssc(x).unwrap();
        let ssc_a = get_ssc(a);
        let ssc_b = get_ssc(b);
        let ssc_c = get_ssc(c);
        let ssc_d = get_ssc(d);
        let ssc_e = get_ssc(e);
        assert!(ssc_a == ssc_b);
        assert!(ssc_b != ssc_c);
        assert!(ssc_c != ssc_d);
        assert!(ssc_b != ssc_d);
        assert!(ssc_b != ssc_e);
    }
}