use std::fmt::Write;
use yaboc_base::{dbformat, error::SResult};
use yaboc_len::len_graph;

use crate::Constraints;
pub fn len_dot<DB: Constraints + Sized>(db: &DB) -> SResult<String> {
    let mut ret = String::from("digraph {\nnode [shape=record];\nrankdir=LR;\n");
    for pd in db.all_parserdefs() {
        let terms = db.len_term(pd).unwrap();
        let vals = db.len_vals(pd);
        let name = dbformat!(db, "{}", &pd.0);
        let prefix = name.replace(|c: char| !c.is_ascii_alphanumeric(), "_");
        write!(ret, "subgraph cluster_{prefix} {{\nlabel=\"{name}\";\n").unwrap();
        let graph = len_graph(&prefix, &terms.expr, &vals.vals, terms.root, &vals.deps);
        ret.push_str(&graph);
        ret.push_str("}\n");
    }
    ret.push_str("}\n");
    Ok(ret)
}
