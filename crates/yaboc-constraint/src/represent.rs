use std::fmt::Write;
use yaboc_backtrack::Matrix;
use yaboc_base::{dbformat, dbwrite, error::SResult};
use yaboc_len::len_graph;

use crate::Constraints;
pub fn len_dot<DB: Constraints + ?Sized>(db: &DB) -> SResult<String> {
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

pub fn backtrack<DB: Constraints + ?Sized>(db: &DB) -> SResult<String> {
    let mut ret = String::new();
    for pd in db.all_parserdefs() {
        let terms = db.bt_term(pd)?;
        let vals = db.bt_vals(pd);
        dbwrite!(ret, db, "Backtrack for {}\n", &pd.0).unwrap();
        for (i, term) in terms.expr.iter().enumerate() {
            dbwrite!(ret, db, "{i}: {}\n", term).unwrap();
            let matrix = Matrix::from_rows(&vals.present[term.row_range()]);
            writeln!(ret, "present:\n{}", matrix).unwrap();
            let matrix = Matrix::from_rows(&vals.forbidden[term.row_range()]);
            writeln!(ret, "forbidden:\n{}", matrix).unwrap();
        }
    }
    Ok(ret)
}
