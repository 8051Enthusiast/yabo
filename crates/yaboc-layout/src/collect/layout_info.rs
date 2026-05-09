use fxhash::FxHashMap;
use yaboc_base::interner::Regex;
use yaboc_constraint::BtTerm;
use yaboc_hir::{BlockId, DefKind, HirConstraintId, HirIdWrapper, LambdaId, ParserDefId};
use yaboc_req::{NeededBy, RequirementSet};

use crate::{FuncLayoutKind, ILayout, IMonoLayout, Layouts, MonoLayout};

#[derive(Hash, PartialEq, Eq, Default, Debug)]
pub struct LayoutInfo {
    can_backtrack: bool,
}

impl LayoutInfo {
    fn merge(&mut self, other: &Self) {
        self.can_backtrack |= other.can_backtrack;
    }

    pub fn modify_reqs(&self, req: RequirementSet) -> RequirementSet {
        if !self.can_backtrack {
            req & !NeededBy::Backtrack
        } else {
            req
        }
    }
}

#[derive(Debug)]
pub struct LayoutInfoCollection<'a> {
    pub info: FxHashMap<ILayout<'a>, LayoutInfo>,
}

pub struct LayoutInfoCollector<'a> {
    info: FxHashMap<ILayout<'a>, LayoutInfo>,
    db: &'a dyn Layouts,
}

impl<'a> LayoutInfoCollector<'a> {
    pub fn new(db: &'a dyn Layouts) -> Self {
        LayoutInfoCollector {
            info: Default::default(),
            db,
        }
    }

    fn get_bt_status(
        &self,
        pd: ParserDefId,
        term_idx: impl Fn(&BtTerm) -> u32,
        row: usize,
    ) -> bool {
        let terms = self.db.bt_term(pd).unwrap();
        let vals = self.db.bt_vals(pd);
        let idx = term_idx(&terms);
        let range = terms.expr[idx as usize].row_range.clone();
        !vals.present[range][row].is_empty()
    }

    fn pd_fun_info(&mut self, pd: ParserDefId) -> LayoutInfo {
        let parserdef = pd.lookup(self.db).unwrap();
        if parserdef.kind != DefKind::Static {
            let can_backtrack = self.get_bt_status(pd, |terms| terms.lookup_idx, 0);
            LayoutInfo { can_backtrack }
        } else {
            LayoutInfo {
                can_backtrack: false,
            }
        }
    }

    fn pd_parser_info(&mut self, pd: ParserDefId) -> LayoutInfo {
        let parserdef = pd.lookup(self.db).unwrap();
        let index = if parserdef.args.is_some() { 1 } else { 0 };
        let can_backtrack = self.get_bt_status(pd, |terms| terms.lookup_idx, index);
        LayoutInfo { can_backtrack }
    }

    fn block_info(&mut self, block: BlockId) -> LayoutInfo {
        let pd = self.db.hir_parent_parserdef(block.0).unwrap();
        let can_backtrack = self.get_bt_status(pd, |terms| terms.blocks[&block], 0);
        LayoutInfo { can_backtrack }
    }

    fn lambda_info(&mut self, lambda: LambdaId) -> LayoutInfo {
        let pd = self.db.hir_parent_parserdef(lambda.0).unwrap();
        let can_backtrack = self.get_bt_status(pd, |terms| terms.lambdas[&lambda], 0);
        LayoutInfo { can_backtrack }
    }

    fn single_info(&mut self) -> LayoutInfo {
        LayoutInfo {
            can_backtrack: false,
        }
    }

    fn regex_info(&mut self, _: Regex) -> LayoutInfo {
        LayoutInfo {
            can_backtrack: true,
        }
    }

    fn if_parser_info(&mut self, inner: ILayout<'a>, _: HirConstraintId) -> LayoutInfo {
        let mut info = LayoutInfo {
            can_backtrack: true,
        };
        info.merge(self.get_info(inner));
        info
    }

    fn array_parser(&mut self) -> LayoutInfo {
        LayoutInfo {
            can_backtrack: false,
        }
    }

    pub fn get_mono_info(&mut self, layout: IMonoLayout<'a>) -> &LayoutInfo {
        if !self.info.contains_key(&layout.inner()) {
            let info = match layout.mono_layout() {
                MonoLayout::Single => self.single_info(),
                MonoLayout::Regex(regex) => self.regex_info(*regex),
                MonoLayout::IfParser(inner, constraint) => self.if_parser_info(*inner, *constraint),
                MonoLayout::ArrayParser(_) => self.array_parser(),
                MonoLayout::ArrayFillParser(_) => self.array_parser(),
                MonoLayout::NominalParser(pd, _, FuncLayoutKind::Fun) => self.pd_fun_info(*pd),
                MonoLayout::NominalParser(pd, _, FuncLayoutKind::Parse) => self.pd_parser_info(*pd),
                MonoLayout::BlockParser(block_id, _) => self.block_info(*block_id),
                MonoLayout::Lambda(lid, _, _) => self.lambda_info(*lid),
                MonoLayout::Block(..)
                | MonoLayout::Array { .. }
                | MonoLayout::Nominal(..)
                | MonoLayout::Primitive(_)
                | MonoLayout::Ptr
                | MonoLayout::SlicePtr
                | MonoLayout::Range => {
                    panic!("Non-functional argument passed to layout info")
                }
            };
            self.info.insert(layout.inner(), info);
        }
        &self.info[&layout.inner()]
    }

    pub fn get_info(&mut self, layout: ILayout<'a>) -> &LayoutInfo {
        if !self.info.contains_key(&layout) {
            let mut res = LayoutInfo {
                can_backtrack: false,
            };
            for mono in &layout {
                let mono_info = self.get_mono_info(mono);
                res.merge(mono_info);
            }

            if layout.is_multi() {
                self.info.insert(layout, res);
            }
        }
        &self.info[&layout]
    }

    pub fn collect(self) -> LayoutInfoCollection<'a> {
        LayoutInfoCollection { info: self.info }
    }
}
