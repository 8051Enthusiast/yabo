use yaboc_base::{dbformat, error::SResult};
use yaboc_len::len_graph;

use crate::Constraints;
pub fn len_dot<DB: Constraints + Sized>(db: &DB) -> SResult<String> {
    let mut ret = String::from("digraph {\nnode [shape=record];\nrankdir=LR;\n");
    for pd in db.all_parserdefs() {
        let terms = db.len_term(pd)?;
        let vals = db.len_vals(pd);
        let prefix = dbformat!(db, "{}", &pd.0).replace(|c: char| !c.is_ascii_alphanumeric(), "_");
        let arg_count = db.argnum(pd)?.unwrap_or_default();
        let deps = terms.expr.static_arg_deps(arg_count);
        let graph = len_graph(
            &prefix,
            &terms.expr.terms,
            &vals.vals,
            &terms.call_arities,
            &deps,
            arg_count,
        );
        ret.push_str(&graph);
    }
    ret.push_str("}\n");
    Ok(ret)
}
