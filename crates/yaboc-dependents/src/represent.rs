use std::fmt::{Display, Write};

use fxhash::FxHashMap;
use yaboc_base::{databased_display::DatabasedDisplay, dbformat, dbwrite, error::SResult};

use crate::{requirements::RequirementMatrix, DepType, DependencyGraph, SubValueKind};

use super::{Dependents, SubValue};

impl Display for SubValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bt => write!(f, "bt"),
            Self::Val => write!(f, "val"),
            Self::Front => write!(f, "front"),
            Self::Back => write!(f, "back"),
        }
    }
}

impl<DB: Dependents + ?Sized> DatabasedDisplay<DB> for SubValue {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        dbwrite!(f, db, "{}({})", &self.kind, &self.id)
    }
}

impl Display for RequirementMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, bit) in Self::id().iter_cols().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            for column in self.iter_cols() {
                if column.contains(bit) {
                    write!(f, "1")?;
                } else {
                    write!(f, "0")?;
                }
            }
        }
        Ok(())
    }
}

impl<DB: Dependents + ?Sized> DatabasedDisplay<DB> for DependencyGraph {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let ids: FxHashMap<_, _> = self
            .val_map
            .keys()
            .map(|id| {
                let ident_str =
                    dbformat!(db, "{}", &id.id).replace(|c: char| !c.is_ascii_alphanumeric(), "_");
                (id.id, ident_str)
            })
            .collect();
        let [reachable_val, reachable_back, reachable_bt] = self.reachables();
        let tails = self.tail_returns(db).unwrap_or_default();
        for (&id, ident_str) in ids.iter() {
            writeln!(f, "\tsubgraph cluster_{ident_str} {{")?;
            dbwrite!(f, db, "\t\tlabel=\"{}\";\n", &id)?;
            for kind in SubValueKind::all() {
                let subval = SubValue { id, kind };
                let color = [&reachable_val, &reachable_back, &reachable_bt]
                    .map(|set| if set.contains(&subval) { "ff" } else { "a8" })
                    .concat();
                if self.val_map.get(&subval).is_some() {
                    write!(
                        f,
                        "\t\t{ident_str}_{kind} [label=\"{kind}\", color=black fillcolor=\"#{color}\" style=filled",
                    )?;
                    if tails.contains(&id) {
                        write!(f, ",shape=\"doubleoctagon\"")?;
                    }
                    writeln!(f, "];")?;
                };
            }
            writeln!(f, "\t}}")?;
        }
        for edge in self.graph.edge_indices() {
            let dotted = *self.graph.edge_weight(edge).unwrap() == DepType::Control;
            let (from, to) = self.graph.edge_endpoints(edge).unwrap();
            let from = self.graph[from];
            let to = self.graph[to];
            let from_ident = ids[&from.id].clone();
            let to_ident = ids[&to.id].clone();
            if dotted {
                writeln!(
                    f,
                    "\t{}_{} -> {}_{} [style=dotted];",
                    from_ident, from.kind, to_ident, to.kind
                )?;
            } else {
                writeln!(
                    f,
                    "\t{}_{} -> {}_{};",
                    from_ident, from.kind, to_ident, to.kind
                )?;
            }
        }
        Ok(())
    }
}

pub fn dependency_dot(db: &impl Dependents) -> SResult<String> {
    let mut ret = String::new();
    ret.push_str(
        r##"digraph dependencies {
        graph [ style="filled,rounded", color="#cccccc", fillcolor="#f0f0f0" ]
    "##,
    );
    let pds = db.all_parserdefs();
    for pd in pds {
        let blocks = db.all_parserdef_blocks(pd);
        for block in blocks.iter() {
            let graph = DependencyGraph::new(db, *block)?;
            let _ = dbwrite!(&mut ret, db, "{}", &graph);
        }
    }
    ret.push_str("}\n");
    Ok(ret)
}
