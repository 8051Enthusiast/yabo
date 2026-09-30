use fxhash::FxHashMap;
use yaboc_base::interner::Regex;
use yaboc_constraint::BtTerm;
use yaboc_hir::{BlockId, DefKind, HirConstraintId, HirIdWrapper, LambdaId, ParserDefId};
use yaboc_mir::MirKind;
use yaboc_req::{NeededBy, RequirementSet};

use crate::{FuncLayoutKind, ILayout, IMonoLayout, Layouts, MonoLayout};

#[derive(Hash, Clone, Copy, PartialEq, Eq, Default, Debug)]
pub enum Length {
    #[default]
    None,
    Const(u64),
    Unsized,
}

impl Length {
    fn from_val<T>(val: &yaboc_len::Val<T>) -> Self {
        match val {
            yaboc_len::Val::Undefined => Length::None,
            &yaboc_len::Val::Const(_, c, _) if let Ok(c) = c.try_into() => Length::Const(c),
            _ => Length::Unsized,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum EvalType {
    NoValue,
    Value,
    Force,
}

impl EvalType {
    pub fn is_val(self) -> bool {
        match self {
            EvalType::NoValue => false,
            EvalType::Value | EvalType::Force => true,
        }
    }

    pub fn is_force(self) -> bool {
        match self {
            EvalType::Value | EvalType::NoValue => false,
            EvalType::Force => true,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct LCallReq {
    pub val: EvalType,
    pub len: bool,
    pub bt: bool,
}

impl std::fmt::Display for LCallReq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let val = match self.val {
            EvalType::NoValue => "_",
            EvalType::Value => "v",
            EvalType::Force => "f",
        };
        let len = match self.len {
            false => "_",
            true => "l",
        };
        let bt = match self.bt {
            false => "_",
            true => "b",
        };
        write!(f, "{}{}{}", val, len, bt)
    }
}

impl LCallReq {
    pub fn remove_bt(self) -> Self {
        LCallReq { bt: false, ..self }
    }

    pub fn remove_len(self) -> Self {
        LCallReq { len: false, ..self }
    }

    pub fn remove_val(self) -> Self {
        LCallReq {
            val: EvalType::NoValue,
            ..self
        }
    }

    pub fn is_empty(self) -> bool {
        !self.len && !self.bt && matches!(self.val, EvalType::NoValue)
    }

    fn as_reqset(self) -> RequirementSet {
        let mut req = RequirementSet::default();
        if self.len {
            req |= NeededBy::Len
        }
        if self.bt {
            req |= NeededBy::Backtrack
        }
        if !matches!(self.val, EvalType::NoValue) {
            req |= NeededBy::Val
        }
        req
    }

    pub fn as_mir_call(self) -> MirKind {
        MirKind::Call(self.as_reqset())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct LCallMeta {
    pub req: LCallReq,
    pub tail: bool,
}

impl std::fmt::Display for LCallMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = if self.tail {
            "tail "
        } else {
            Default::default()
        };
        write!(f, "{}{}", prefix, self.req)
    }
}

#[derive(Hash, PartialEq, Eq, Default, Debug)]
pub struct LayoutInfo {
    can_backtrack: bool,
    len: Length,
    is_thunky: bool,
}

impl LayoutInfo {
    pub fn modify_reqs(&self, mut req: LCallReq) -> (LCallReq, Option<u64>) {
        req = if !self.can_backtrack {
            req.remove_bt()
        } else {
            req
        };
        let mut needs_length_precheck = None;
        req = if let Length::Const(len) = self.len
            && !req.bt
            && req.len
        {
            needs_length_precheck = Some(len);
            req.remove_len()
        } else {
            req
        };
        if self.is_thunky && !req.val.is_force() {
            req = req.remove_val();
        }
        (req, needs_length_precheck)
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
        if parserdef.kind == DefKind::Static {
            LayoutInfo {
                can_backtrack: false,
                len: Length::None,
                is_thunky: false,
            }
        } else {
            let can_backtrack = self.get_bt_status(pd, |terms| terms.lookup_idx, 0);
            LayoutInfo {
                can_backtrack,
                len: Length::None,
                is_thunky: parserdef.kind.thunky(),
            }
        }
    }

    fn pd_parser_info(&mut self, pd: ParserDefId) -> LayoutInfo {
        let parserdef = pd.lookup(self.db).unwrap();
        let index = if parserdef.args.is_some() { 1 } else { 0 };
        let can_backtrack = self.get_bt_status(pd, |terms| terms.lookup_idx, index);
        let len_terms = self.db.len_vals(pd);
        let root_val = &len_terms.fun_val;
        LayoutInfo {
            can_backtrack,
            len: Length::from_val(root_val),
            is_thunky: parserdef.kind.thunky(),
        }
    }

    fn block_info(&mut self, block: BlockId) -> LayoutInfo {
        let pd = self.db.hir_parent_parserdef(block.0).unwrap();
        let kind = block.lookup(self.db).unwrap().kind;
        let can_backtrack = self.get_bt_status(pd, |terms| terms.blocks[&block], 0);
        let len = match kind {
            yaboc_hir::BlockKind::Parser => {
                let len_vals = self.db.len_vals(pd);
                let len_terms = self.db.len_term(pd).unwrap();
                let v = &len_vals.vals[len_terms.block_locs[&block]];
                Length::from_val(v)
            }
            yaboc_hir::BlockKind::Inline => Length::None,
        };
        LayoutInfo {
            can_backtrack,
            len,
            is_thunky: false,
        }
    }

    fn lambda_info(&mut self, lambda: LambdaId) -> LayoutInfo {
        let pd = self.db.hir_parent_parserdef(lambda.0).unwrap();
        let can_backtrack = self.get_bt_status(pd, |terms| terms.lambdas[&lambda], 0);
        LayoutInfo {
            can_backtrack,
            len: Length::None,
            is_thunky: false,
        }
    }

    fn single_info(&mut self) -> LayoutInfo {
        LayoutInfo {
            can_backtrack: false,
            len: Length::Const(1),
            is_thunky: false,
        }
    }

    fn regex_info(&mut self, regex: Regex) -> LayoutInfo {
        let len = if let Ok(Some(reg_len)) = self.db.regex_len(regex)
            && let Ok(len) = reg_len.try_into()
        {
            Length::Const(len)
        } else {
            Length::Unsized
        };
        LayoutInfo {
            can_backtrack: true,
            len,
            is_thunky: false,
        }
    }

    fn if_parser_info(&mut self, inner: ILayout<'a>, _: HirConstraintId) -> LayoutInfo {
        let mut info = LayoutInfo {
            can_backtrack: true,
            len: Length::None,
            is_thunky: false,
        };
        for mono in &inner {
            let mono_info = self.get_mono_info(mono);
            info.len = match (info.len, mono_info.len) {
                (a, Length::None) | (Length::None, a) => a,
                (Length::Const(a), Length::Const(b)) if a == b => Length::Const(a),
                _ => Length::Unsized,
            };
        }
        info
    }

    fn array_parser(&mut self) -> LayoutInfo {
        LayoutInfo {
            can_backtrack: false,
            len: Length::Unsized,
            is_thunky: false,
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

    pub fn collect(self) -> LayoutInfoCollection<'a> {
        LayoutInfoCollection { info: self.info }
    }
}
