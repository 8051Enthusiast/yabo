mod call_info;
mod tailsize;

use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};

use yaboc_absint::{AbstractDomain, Arg};
use yaboc_base::dbeprintln;
use yaboc_hir::{HirIdWrapper, ParserDefId};
use yaboc_hir_types::DerefLevel;
use yaboc_mir::{CallMeta, MirInstr, MirKind, Place};
use yaboc_req::{NeededBy, RequirementSet};
use yaboc_target::layout::{PSize, SizeAlign};
use yaboc_types::{PrimitiveType, Type, TypeId};

use crate::{
    init_globals,
    mir_subst::{function_substitute, FunctionSubstitute},
    LayoutSlice,
};

pub use self::tailsize::TailInfo;
use self::tailsize::{CallSite, TailCollector};

use super::{canon_layout, AbsLayoutCtx, ILayout, IMonoLayout, Layout, LayoutError, MonoLayout};

const TRACE_COLLECTION: bool = false;

type LayoutSet<'a> = FxHashSet<IMonoLayout<'a>>;

pub fn pd_len_req() -> CallMeta {
    CallMeta::new(NeededBy::Len.into(), false)
}
pub fn pd_val_req() -> CallMeta {
    CallMeta::new(NeededBy::Val.into(), false)
}
pub fn fun_req() -> RequirementSet {
    NeededBy::Val | NeededBy::Backtrack
}
pub fn root_req() -> CallMeta {
    CallMeta::new(NeededBy::Val | NeededBy::Len | NeededBy::Backtrack, false)
}
pub fn apply_req() -> CallMeta {
    CallMeta::new(NeededBy::Val | NeededBy::Backtrack, false)
}

#[derive(Debug)]
pub struct LayoutCollection<'a> {
    pub root: Vec<(IMonoLayout<'a>, PSize)>,
    pub arrays: LayoutSet<'a>,
    pub blocks: LayoutSet<'a>,
    pub nominals: LayoutSet<'a>,
    pub parsers: LayoutSet<'a>,
    pub functions: LayoutSet<'a>,
    pub primitives: LayoutSet<'a>,
    pub lens: LayoutSet<'a>,
    pub globals: FxHashMap<ParserDefId, (IMonoLayout<'a>, ILayout<'a>)>,
    pub parser_slots: call_info::CallSlotResult<'a, (ILayout<'a>, CallMeta)>,
    pub funcall_slots: call_info::CallSlotResult<'a, LayoutSlice<'a>>,
    pub tail_sa: FxHashMap<(ILayout<'a>, IMonoLayout<'a>), TailInfo>,
    pub max_sa: SizeAlign,
}

pub struct LayoutCollector<'a, 'b> {
    ctx: &'b mut AbsLayoutCtx<'a>,
    int: ILayout<'a>,
    parses: call_info::CallInfo<'a, (ILayout<'a>, CallMeta)>,
    funcalls: call_info::CallInfo<'a, LayoutSlice<'a>>,
    arrays: LayoutSet<'a>,
    blocks: LayoutSet<'a>,
    nominals: LayoutSet<'a>,
    parsers: LayoutSet<'a>,
    functions: LayoutSet<'a>,
    lens: LayoutSet<'a>,
    globals: FxHashMap<ParserDefId, (IMonoLayout<'a>, ILayout<'a>)>,
    root: Vec<(ILayout<'a>, IMonoLayout<'a>)>,
    max_sa: SizeAlign,
    processed_calls: FxHashSet<(ILayout<'a>, IMonoLayout<'a>, CallMeta)>,
    unprocessed: Vec<UnprocessedCall<'a>>,
}

#[derive(Debug)]
pub enum UnprocessedCall<'a> {
    NominalParser(ILayout<'a>, IMonoLayout<'a>, MirKind),
    NominalEvalFun(IMonoLayout<'a>),
    LambdaEvalFun(IMonoLayout<'a>),
    BlockParser(ILayout<'a>, IMonoLayout<'a>, MirKind),
    BlockEvalFun(IMonoLayout<'a>),
    IfParser(ILayout<'a>, IMonoLayout<'a>, MirKind),
}

impl<'a, 'b> LayoutCollector<'a, 'b> {
    pub fn new(ctx: &'b mut AbsLayoutCtx<'a>) -> Self {
        let int = ctx.dcx.int(ctx.db);
        // even if no layout of ours has a vtable pointer, the caller may still request
        // one
        let max_sa = SizeAlign::default().tac(ctx.dcx.target_data.pointer_sa);
        LayoutCollector {
            ctx,
            int,
            parses: Default::default(),
            funcalls: Default::default(),
            arrays: Default::default(),
            blocks: Default::default(),
            nominals: Default::default(),
            parsers: Default::default(),
            functions: Default::default(),
            lens: Default::default(),
            globals: Default::default(),
            max_sa,
            processed_calls: Default::default(),
            unprocessed: Default::default(),
            root: Default::default(),
        }
    }

    fn register_function(&mut self, mono: IMonoLayout<'a>) {
        let eval = match mono.mono_layout().0 {
            MonoLayout::NominalParser(pd, args, _) => {
                let parserdef = pd.lookup(self.ctx.db).unwrap();
                let def_arg_count = parserdef.args.map(|x| x.len()).unwrap_or(0);
                (args.len() == def_arg_count && parserdef.from.is_none())
                    .then_some(UnprocessedCall::NominalEvalFun as fn(_) -> _)
            }
            MonoLayout::BlockParser(..) => Some(UnprocessedCall::BlockEvalFun as fn(_) -> _),
            MonoLayout::Lambda(lambda_id, _, args, ..) => {
                let lambda = lambda_id.lookup(self.ctx.db).unwrap();
                let def_arg_count = lambda.args.len();
                (args.len() == def_arg_count)
                    .then_some(UnprocessedCall::LambdaEvalFun as fn(_) -> _)
            }
            _ => None,
        };
        for bt_status in mono.backtrack_statuses(self.ctx) {
            if self.functions.insert(bt_status) {
                if TRACE_COLLECTION {
                    dbeprintln!(self.ctx.db, "[collection] registered function {}", &mono);
                }
                if let Some(eval) = eval {
                    self.unprocessed.push(eval(bt_status));
                }
            }
        }
    }

    fn register_parser_or_function(&mut self, mono: IMonoLayout<'a>) {
        let ty = self.ctx.db.lookup_intern_type(mono.mono_layout().1);
        if let Type::ParserArg { .. } = ty {
            for bt_status in mono.backtrack_statuses(self.ctx) {
                if self.parsers.insert(bt_status) && TRACE_COLLECTION {
                    dbeprintln!(self.ctx.db, "[collection] registered parser {}", &mono);
                }
            }
        } else {
            self.register_function(mono);
        }
    }

    fn register_layouts(&mut self, layout: ILayout<'a>) {
        for mono in &layout {
            if let Ok(sa) = mono.inner().size_align_without_vtable(self.ctx) {
                self.max_sa = self.max_sa.union(sa);
            }
            match &mono.mono_layout().0 {
                MonoLayout::SlicePtr | MonoLayout::Range => {
                    if self.arrays.insert(mono) && TRACE_COLLECTION {
                        dbeprintln!(self.ctx.db, "[collection] registered array {}", &mono);
                    }
                }
                MonoLayout::Array { parser, slice } => {
                    if self.arrays.insert(mono) && TRACE_COLLECTION {
                        dbeprintln!(self.ctx.db, "[collection] registered array {}", &mono);
                    }
                    self.register_layouts(*slice);
                    self.register_layouts(*parser);
                    self.register_parse(*slice, *parser, pd_val_req());
                    self.register_len(*parser);
                }
                MonoLayout::Nominal(pd, _, _) => {
                    let parserdef = pd.lookup(self.ctx.db).unwrap();
                    if parserdef.kind.thunky() && self.nominals.insert(mono) {
                        if TRACE_COLLECTION {
                            dbeprintln!(self.ctx.db, "[collection] registered nominal {}", &mono);
                        }
                        let (arg, parser) = mono.unapply_nominal(self.ctx);
                        let parser = parser.inner();
                        // the original parser may have been backtracking, so we need to register
                        // the corresponding non-backtracking parser as well
                        self.register_layouts(parser);
                        self.register_parse(arg, parser, pd_val_req());
                        self.register_parse(arg, parser, pd_len_req());
                    }
                }
                MonoLayout::Block(_, _) => {
                    if self.blocks.insert(mono) && TRACE_COLLECTION {
                        dbeprintln!(self.ctx.db, "[collection] registered block {}", &mono);
                    }
                }
                MonoLayout::NominalParser(_, _, _)
                | MonoLayout::Lambda(..)
                | MonoLayout::BlockParser(..)
                | MonoLayout::ArrayFillParser(Some(_)) => {
                    self.register_parser_or_function(mono);
                }
                MonoLayout::ArrayParser(Some((inner_parser, Some(_)))) => {
                    self.register_parser_or_function(mono);
                    self.register_len(*inner_parser);
                }
                MonoLayout::Single | MonoLayout::IfParser(..) | MonoLayout::Regex(..) => {
                    for bt_status in mono.backtrack_statuses(self.ctx) {
                        if self.parsers.insert(bt_status) && TRACE_COLLECTION {
                            dbeprintln!(self.ctx.db, "[collection] registered parser {}", &mono);
                        }
                    }
                }
                MonoLayout::ArrayParser(Some((_, None)) | None)
                | MonoLayout::ArrayFillParser(None) => {
                    for bt_status in mono.backtrack_statuses(self.ctx) {
                        if self.functions.insert(bt_status) && TRACE_COLLECTION {
                            dbeprintln!(self.ctx.db, "[collection] registered function {}", &mono);
                        }
                    }
                }
                MonoLayout::Primitive(_) => {}
            }
        }
    }

    fn register_parse(&mut self, arg: ILayout<'a>, parser: ILayout<'a>, info: CallMeta) {
        if info.req.is_empty() {
            return;
        }
        let parser = parser
            .deref_to_level(self.ctx, DerefLevel::zero())
            .unwrap()
            .0;
        for mono in &parser {
            match mono.mono_layout().0 {
                MonoLayout::BlockParser(..) => {
                    if self.processed_calls.insert((arg, mono, info)) {
                        if TRACE_COLLECTION {
                            dbeprintln!(
                                self.ctx.db,
                                "[collection] registered block parse({}) {} ~> {}",
                                &info,
                                &arg,
                                &mono
                            );
                        }
                        self.unprocessed.push(UnprocessedCall::BlockParser(
                            arg,
                            mono,
                            MirKind::Call(info.req),
                        ));
                    }
                }
                MonoLayout::NominalParser(..) => {
                    if self.processed_calls.insert((arg, mono, info)) {
                        if TRACE_COLLECTION {
                            dbeprintln!(
                                self.ctx.db,
                                "[collection] registered nominal parse({}) {} ~> {}",
                                &info,
                                &arg,
                                &mono
                            );
                        }
                        self.unprocessed.push(UnprocessedCall::NominalParser(
                            arg,
                            mono,
                            MirKind::Call(info.req),
                        ));
                    }
                }
                MonoLayout::Regex(..) => {
                    let single = IMonoLayout::u8_single(self.ctx);
                    self.register_layouts(single.inner());
                    self.parses.add_call(
                        (arg, CallMeta::new(RequirementSet::all(), false)),
                        single.inner(),
                    );
                }
                MonoLayout::IfParser(..) => {
                    if self.processed_calls.insert((arg, mono, info)) {
                        if TRACE_COLLECTION {
                            dbeprintln!(
                                self.ctx.db,
                                "[collection] registered if parse({}) {} ~> {}",
                                &info,
                                &arg,
                                &mono
                            );
                        }
                        self.unprocessed.push(UnprocessedCall::IfParser(
                            arg,
                            mono,
                            MirKind::Call(info.req),
                        ));
                    }
                }
                MonoLayout::ArrayParser(..) | MonoLayout::ArrayFillParser(..) => {
                    let result = parser.apply_arg(self.ctx, arg).unwrap();
                    self.register_layouts(result);
                }
                _ => {}
            }
        }
        self.parses.add_call((arg, info), parser)
    }

    fn register_funcall(
        &mut self,
        fun: ILayout<'a>,
        args: Vec<ILayout<'a>>,
        fun_ty: TypeId,
    ) -> Result<(), LayoutError> {
        let Type::FunctionArg(_, arg_tys) = self.ctx.db.lookup_intern_type(fun_ty) else {
            panic!("register_funcall called with non-function type");
        };
        let casted_args = args
            .iter()
            .zip(arg_tys.iter())
            .map(|(l, ty)| Ok(l.typecast(self.ctx, *ty)?.0))
            .collect::<Result<Vec<_>, LayoutError>>()?;
        let arg_tuple = self.ctx.dcx.intern_slice.intern_slice(&casted_args);
        if TRACE_COLLECTION {
            dbeprintln!(
                self.ctx.db,
                "[collection] registered funcall {}({})",
                &fun,
                &arg_tuple.1
            );
        }
        self.funcalls.add_call(arg_tuple, fun);
        Ok(())
    }

    fn parser_len_proc_entry(&self, parser: IMonoLayout<'a>) -> Option<UnprocessedCall<'a>> {
        match parser.mono_layout().0 {
            MonoLayout::NominalParser(..) => Some(UnprocessedCall::NominalParser(
                self.int,
                parser,
                MirKind::Len,
            )),
            MonoLayout::IfParser(..) => {
                Some(UnprocessedCall::IfParser(self.int, parser, MirKind::Len))
            }
            MonoLayout::BlockParser(..) => {
                Some(UnprocessedCall::BlockParser(self.int, parser, MirKind::Len))
            }
            _ => None,
        }
    }

    fn register_len(&mut self, parser: ILayout<'a>) {
        for parser in &parser {
            match parser.mono_layout().0 {
                MonoLayout::SlicePtr | MonoLayout::Range | MonoLayout::Array { .. } => continue,
                _ => {}
            }
            if self.lens.insert(parser) {
                if TRACE_COLLECTION {
                    dbeprintln!(self.ctx.db, "[collection] registered len {}", &parser);
                }
                if let Some(call) = self.parser_len_proc_entry(parser) {
                    self.unprocessed.push(call);
                }
            }
        }
    }

    fn register_global(&mut self, pd: ParserDefId) {
        if let Entry::Vacant(v) = self.globals.entry(pd) {
            if TRACE_COLLECTION {
                dbeprintln!(self.ctx.db, "[collection] registered global {}", &pd.0);
            }
            let entries @ (fun, layout) = self.ctx.dcx.globals[&pd];
            v.insert(entries);
            self.register_layouts(fun.inner());
            self.register_layouts(layout);
        }
    }

    fn collect_ins(
        &mut self,
        mir: &FunctionSubstitute<'a>,
        ins: MirInstr,
    ) -> Result<(), LayoutError> {
        match ins {
            MirInstr::ApplyArgs(_, fun, args, _, _) => {
                let fun_layout = mir.place(fun);
                let args = args
                    .iter()
                    .map(|(arg, _)| mir.place(*arg))
                    .collect::<Vec<_>>();
                let ty = mir.place_type(fun);
                self.register_funcall(fun_layout, args, ty)?;
                Ok(())
            }
            MirInstr::ParseCall(_, _, meta, arg, fun, _) => {
                let fun_layout = mir.place(fun);
                let arg_layout = mir.place(arg);
                self.register_parse(arg_layout, fun_layout, meta);
                Ok(())
            }
            MirInstr::LenCall(_, fun, _) => {
                let fun_layout = mir.place(fun);
                self.register_len(fun_layout);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn collect_mir(&mut self, mir: &FunctionSubstitute<'a>) -> Result<(), LayoutError> {
        if TRACE_COLLECTION {
            eprintln!("[collection] collecting mir");
        }
        for layout in mir.stack_layouts.iter() {
            self.register_layouts(*layout);
        }
        for (_, place) in mir.f.iter_places() {
            if let Place::Global(pd) = &place.place {
                self.register_global(*pd);
            }
        }
        for ins in mir.f.iter_bb().flat_map(|(_, block)| block.ins()) {
            self.collect_ins(mir, ins)?;
        }
        if TRACE_COLLECTION {
            eprintln!("[collection] finished collecting mir");
        }
        Ok(())
    }
    fn collect_block_parse(
        &mut self,
        arg: ILayout<'a>,
        parser: IMonoLayout<'a>,
        mut info: MirKind,
    ) -> Result<(), LayoutError> {
        if TRACE_COLLECTION {
            dbeprintln!(
                self.ctx.db,
                "[collection] processing block parser({}) {} ~> {}",
                &info,
                &arg,
                &parser.inner()
            );
        }
        let MonoLayout::BlockParser(id, _, _, bt) = parser.mono_layout().0 else {
            panic!("unexpected non-block-parser layout");
        };
        if !*bt {
            if let MirKind::Call(req) = &mut info {
                *req &= !NeededBy::Backtrack;
            }
        };
        parser.inner().apply_arg(self.ctx, arg)?;
        let fsub = function_substitute(
            yaboc_mir::FunKind::Block(*id),
            info,
            Some(arg),
            parser,
            self.ctx,
        )?;
        self.collect_mir(&fsub)?;
        Ok(())
    }

    fn collect_block_fun(&mut self, parser: IMonoLayout<'a>) -> Result<(), LayoutError> {
        if TRACE_COLLECTION {
            dbeprintln!(
                self.ctx.db,
                "[collection] processing block eval fun {}",
                &parser
            );
        }
        let MonoLayout::BlockParser(id, _, _, _) = parser.mono_layout().0 else {
            panic!("unexpected non-block-parser layout");
        };
        parser.inner().eval_fun(self.ctx)?;
        let req = fun_req();
        let fsub = function_substitute(
            yaboc_mir::FunKind::Block(*id),
            MirKind::Call(req),
            None,
            parser,
            self.ctx,
        )?;
        self.collect_mir(&fsub)?;
        Ok(())
    }

    fn collect_nominal_parse(
        &mut self,
        arg: ILayout<'a>,
        parser: IMonoLayout<'a>,
        mut info: MirKind,
    ) -> Result<(), LayoutError> {
        if TRACE_COLLECTION {
            dbeprintln!(
                self.ctx.db,
                "[collection] processing nominal parser({}) {} ~> {}",
                &info,
                &arg,
                &parser.inner()
            );
        }
        let (MonoLayout::NominalParser(pd, thunk_args, bt), ty) = parser.mono_layout() else {
            panic!("unexpected non-nominal-parser layout");
        };
        let Type::ParserArg { arg: arg_ty, .. } = self.ctx.db.lookup_intern_type(ty) else {
            panic!("unexpected non-parserarg type");
        };
        if !*bt {
            if let MirKind::Call(req) = &mut info {
                *req &= !NeededBy::Backtrack;
            }
        }
        let parserdef = pd.lookup(self.ctx.db).unwrap();
        let mut args = FxHashMap::default();
        let thunk_ty = self.ctx.db.parser_result(ty).unwrap();
        args.insert(Arg::From, (arg, arg_ty));
        let arg_ids = parserdef.args.unwrap_or_default();
        for (id, arg) in arg_ids.iter().zip(thunk_args.iter()) {
            args.insert(Arg::Named(id.0), *arg);
        }
        let thunk = IMonoLayout::make_thunk(*pd, thunk_ty, &args, self.ctx).unwrap();
        // it may happen that the thunk does not actually exist as a value
        // anywhere in the program because it always gets immediately evaluated,
        // but we need to register it anyway because it is needed for codegen
        if info != MirKind::Len && parserdef.kind.thunky() {
            self.register_layouts(thunk.inner());
        }
        // instantiate info for thunk
        thunk.deref(self.ctx)?;
        let (arg_layout, parser_layout) = thunk.unapply_nominal(self.ctx);
        // for each parse, it is possible that the value is not actually
        // needed and just a thunk is returned, which means we also need
        // to collect the parse that does not return the value
        let val_info = if let MirKind::Call(req) = info {
            if req.contains(NeededBy::Val) {
                Some(MirKind::Call(req & !NeededBy::Val))
            } else {
                None
            }
        } else {
            None
        };
        for info in std::iter::once(info).chain(val_info) {
            let fsub = function_substitute(
                yaboc_mir::FunKind::ParserDef(*pd),
                info,
                Some(arg_layout),
                parser_layout,
                self.ctx,
            )?;
            self.collect_mir(&fsub)?;
        }
        Ok(())
    }

    fn collect_nominal_eval_fun(&mut self, fun: IMonoLayout<'a>) -> Result<(), LayoutError> {
        if TRACE_COLLECTION {
            dbeprintln!(
                self.ctx.db,
                "[collection] processing nominal eval fun {}",
                &fun
            );
        }
        let (MonoLayout::NominalParser(pd, _, _), _) = fun.mono_layout() else {
            panic!("unexpected non-nominal-parser layout");
        };
        let req = fun_req();
        let fsub = function_substitute(
            yaboc_mir::FunKind::ParserDef(*pd),
            MirKind::Call(req),
            None,
            fun,
            self.ctx,
        )?;
        self.collect_mir(&fsub)?;
        Ok(())
    }

    fn collect_lambda_eval_fun(&mut self, fun: IMonoLayout<'a>) -> Result<(), LayoutError> {
        if TRACE_COLLECTION {
            dbeprintln!(
                self.ctx.db,
                "[collection] processing lambda eval fun {}",
                &fun
            );
        }
        let (MonoLayout::Lambda(lambda_id, ..), _) = fun.mono_layout() else {
            panic!("unexpected non-lambda layout");
        };
        fun.inner().eval_fun(self.ctx)?;
        let fsub = function_substitute(
            yaboc_mir::FunKind::Lambda(*lambda_id),
            MirKind::Call(fun_req()),
            None,
            fun,
            self.ctx,
        )?;
        self.collect_mir(&fsub)?;
        Ok(())
    }

    // if we apply an int to a parser, this actually stems from a len call
    // whose return value gets ignored anyway
    fn skip_call(&self, call: &UnprocessedCall) -> bool {
        match call {
            UnprocessedCall::NominalParser(t, _, MirKind::Call(_))
            | UnprocessedCall::BlockParser(t, _, MirKind::Call(_))
            | UnprocessedCall::IfParser(t, _, MirKind::Call(_)) => t,
            _ => return false,
        }
        .is_int()
    }

    fn proc_list(&mut self) -> Result<(), LayoutError> {
        while let Some(mono) = self.unprocessed.pop() {
            if self.skip_call(&mono) {
                continue;
            }
            match mono {
                UnprocessedCall::BlockParser(arg, parser, info) => {
                    self.collect_block_parse(arg, parser, info)?
                }
                UnprocessedCall::BlockEvalFun(fun) => self.collect_block_fun(fun)?,
                UnprocessedCall::NominalParser(arg, parser, info) => {
                    self.collect_nominal_parse(arg, parser, info)?
                }
                UnprocessedCall::NominalEvalFun(fun) => self.collect_nominal_eval_fun(fun)?,
                UnprocessedCall::LambdaEvalFun(fun) => self.collect_lambda_eval_fun(fun)?,
                UnprocessedCall::IfParser(from, parser, info) => {
                    if TRACE_COLLECTION {
                        dbeprintln!(
                            self.ctx.db,
                            "[collection] processing if-parser({}) {} ~> {}",
                            &info,
                            &from,
                            &parser.inner()
                        );
                    }
                    let MonoLayout::IfParser(inner, c, _) = parser.mono_layout().0 else {
                        panic!("unexpected non-if-parser layout");
                    };
                    self.register_layouts(*inner);
                    if let MirKind::Call(info) = info {
                        let mut first_req = info | NeededBy::Val;
                        if self.ctx.db.lookup_intern_hir_constraint(*c).has_no_eof {
                            first_req |= NeededBy::Len
                        }
                        self.register_parse(from, *inner, CallMeta::new(first_req, false));
                        self.register_parse(
                            from,
                            *inner,
                            CallMeta::new(info & NeededBy::Val, false),
                        );
                    }
                }
            }
        }
        Ok(())
    }

    pub fn collect(&mut self, pds: &[ParserDefId]) -> Result<(), LayoutError> {
        if TRACE_COLLECTION {
            eprintln!("[collection] initializing globals...")
        }
        init_globals(self.ctx)?;
        if TRACE_COLLECTION {
            eprintln!("[collection] collecting layouts for:");
        }
        for pd in pds {
            if TRACE_COLLECTION {
                dbeprintln!(self.ctx.db, "[collection]    parserdef {}", &pd.0);
            }
            let sig = self.ctx.db.parser_args(*pd)?;
            let thunk_ty = self.ctx.db.intern_type(Type::Nominal(sig.thunk));
            let thunk_layout = canon_layout(self.ctx, thunk_ty)?;

            let mut args = Vec::default();
            for arg_ty in sig.args.iter().flat_map(|x| x.iter()) {
                let arg_layout = canon_layout(self.ctx, *arg_ty)?;
                self.register_layouts(arg_layout);
                args.push((arg_layout, *arg_ty));
            }

            if let Some(from) = sig.from {
                let from_layout = canon_layout(self.ctx, from)?;
                self.register_layouts(from_layout);
                let parser_ty = self.ctx.db.intern_type(yaboc_types::Type::ParserArg {
                    result: thunk_ty,
                    arg: from,
                });
                let parser_layout = self.ctx.dcx.intern(Layout::Mono(
                    MonoLayout::NominalParser(*pd, args, true),
                    parser_ty,
                ));
                self.register_layouts(parser_layout);
                for mono in &parser_layout {
                    self.root.push((from_layout, mono));
                }
                self.register_parse(from_layout, parser_layout, root_req());
            }
            self.register_layouts(thunk_layout);
        }
        if TRACE_COLLECTION {
            eprintln!("[collection] ---- starting collection ----");
        }
        self.proc_list()?;
        self.lens.clone_from(&self.parsers);
        for parser in self.parsers.iter() {
            if let Some(parser) = self.parser_len_proc_entry(*parser) {
                self.unprocessed.push(parser);
            }
        }
        if TRACE_COLLECTION {
            eprintln!("[collection] ---- starting len collection ----");
        }
        self.proc_list()?;
        if TRACE_COLLECTION {
            eprintln!("[collection] ---- finished collection ----");
        }
        Ok(())
    }

    pub fn into_results(self) -> Result<LayoutCollection<'a>, LayoutError> {
        let parser_slots = self.parses.into_layout_vtable_offsets();
        let funcall_slots = self.funcalls.into_layout_vtable_offsets();
        let mut primitives = FxHashSet::default();

        for x in [
            PrimitiveType::Int,
            PrimitiveType::Bit,
            PrimitiveType::Char,
            PrimitiveType::Unit,
            PrimitiveType::U8,
        ] {
            let primitive_type = self.ctx.db.intern_type(Type::Primitive(x));
            let layout = self
                .ctx
                .dcx
                .intern(Layout::Mono(MonoLayout::Primitive(x), primitive_type));
            primitives.insert(IMonoLayout(layout));
        }

        let root = self
            .root
            .into_iter()
            .map(|(from, mono)| {
                let key = ((from, root_req()), mono.0);
                let slot = parser_slots.layout_vtable_offsets[&key];
                (mono, slot)
            })
            .collect();

        let mut tail_sa = FxHashMap::default();
        let mut tail_collector = TailCollector::new(self.ctx);
        for (parser, froms) in parser_slots.occupied_entries.iter() {
            for (_, (from, _)) in froms.iter() {
                let sa = tail_collector.size(CallSite(*from, *parser))?;
                tail_sa.insert((*from, *parser), sa);
            }
        }

        Ok(LayoutCollection {
            root,
            arrays: self.arrays,
            blocks: self.blocks,
            nominals: self.nominals,
            parsers: self.parsers,
            functions: self.functions,
            lens: self.lens,
            globals: self.globals,
            max_sa: self.max_sa,
            primitives,
            parser_slots,
            funcall_slots,
            tail_sa,
        })
    }
}

pub fn collected_layouts<'a>(
    ctx: &mut AbsLayoutCtx<'a>,
    pds: &[ParserDefId],
) -> Result<LayoutCollection<'a>, LayoutError> {
    let mut collector = LayoutCollector::new(ctx);
    collector.collect(pds)?;
    collector.into_results()
}
