use fxhash::{FxHashMap, FxHashSet};
use yaboc_hir::HirIdWrapper;
use yaboc_mir::{CallMeta, FunKind, MirInstr, MirKind};
use yaboc_req::RequirementSet;
use yaboc_target::layout::SizeAlign;

use crate::{
    AbsLayoutCtx, ILayout, IMonoLayout, LayoutError, MonoLayout,
    collect::layout_info::LayoutInfoCollector, mir_subst::function_substitute,
};

#[derive(Default, Clone, Copy)]
struct CallSiteVertex {
    index: usize,
    lowlink: usize,
    on_stack: bool,
    sa: SizeAlign,
    has_tailsites: bool,
}

impl CallSiteVertex {
    fn tail_info(&self) -> TailInfo {
        TailInfo {
            has_tailsites: self.has_tailsites,
            sa: self.sa,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TailCallSite<'comp> {
    pub from: Option<ILayout<'comp>>,
    pub func: IMonoLayout<'comp>,
    pub req: RequirementSet,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TailInfo {
    pub has_tailsites: bool,
    pub sa: SizeAlign,
}

pub struct TailCollector<'comp, 'r> {
    ctx: &'r mut AbsLayoutCtx<'comp>,
    info: &'r mut LayoutInfoCollector<'comp>,
    vertices: FxHashMap<TailCallSite<'comp>, CallSiteVertex>,
    stack: Vec<TailCallSite<'comp>>,
    index: usize,
}

impl<'comp, 'r> TailCollector<'comp, 'r> {
    pub fn new(ctx: &'r mut AbsLayoutCtx<'comp>, info: &'r mut LayoutInfoCollector<'comp>) -> Self {
        Self {
            ctx,
            vertices: Default::default(),
            stack: Default::default(),
            index: 0,
            info,
        }
    }

    // go through the mir and find every tail callsite
    // and call f on it
    fn for_each_tail_callsite(
        &mut self,
        site: TailCallSite<'comp>,
        mut f: impl FnMut(&mut Self, TailCallSite<'comp>) -> Result<(), LayoutError>,
    ) -> Result<(), LayoutError> {
        let fun_kind = match site.func.mono_layout() {
            MonoLayout::NominalParser(pd, _, _) => {
                if site.from.is_none() {
                    let parserdef = pd.lookup(self.ctx.db)?;
                    if parserdef.from.is_some() {
                        return Ok(());
                    }
                }
                FunKind::ParserDef(*pd)
            }
            MonoLayout::BlockParser(b, _) => FunKind::Block(*b),
            MonoLayout::Lambda(lid, ..) => FunKind::Lambda(*lid),
            _ => return Ok(()),
        };
        let layout_info = self.info.get_info(site.func.inner());
        let req = layout_info.modify_reqs(site.req);
        let fsub =
            function_substitute(fun_kind, MirKind::Call(req), site.from, site.func, self.ctx)?;
        let mut already_called = FxHashSet::default();
        for instr in fsub.f.iter_bb().flat_map(|(_, bb)| bb.ins()) {
            let (arg, fun, req) = match instr {
                MirInstr::ParseCall(
                    ..,
                    CallMeta {
                        tail: true, req, ..
                    },
                    arg,
                    fun,
                    _,
                ) => (Some(arg), fun, req),
                MirInstr::EvalFun(
                    ..,
                    fun,
                    CallMeta {
                        tail: true, req, ..
                    },
                    _,
                ) => (None, fun, req),
                _ => continue,
            };
            let fun = fsub.place(fun);
            for inner_fun in &fun {
                let inner_site = TailCallSite {
                    from: arg.map(|a| fsub.place(a)),
                    func: inner_fun,
                    req,
                };
                if already_called.insert(inner_site) {
                    f(self, inner_site)?;
                }
            }
        }
        Ok(())
    }

    fn calculate_tail_size(
        &mut self,
        site: TailCallSite<'comp>,
    ) -> Result<CallSiteVertex, LayoutError> {
        let sa = site.func.inner().size_align_without_vtable(self.ctx)?;
        let mut current_vertex = CallSiteVertex {
            index: self.index,
            lowlink: self.index,
            sa,
            has_tailsites: false,
            on_stack: true,
        };
        self.vertices.insert(site, current_vertex);
        self.index += 1;
        self.stack.push(site);

        // we use tarjan's algorithm to find strongly connected components and
        // get the maximum size of the tail call storage
        self.for_each_tail_callsite(site, |this, subsite| {
            current_vertex.has_tailsites = true;
            let subsize = if let Some(&subsite_vertex) = this.vertices.get(&subsite) {
                if subsite_vertex.on_stack {
                    current_vertex.lowlink = current_vertex.lowlink.min(subsite_vertex.index);
                }
                subsite_vertex.sa
            } else {
                let subsite_vertex = this.calculate_tail_size(subsite)?;
                current_vertex.lowlink = current_vertex.lowlink.min(subsite_vertex.lowlink);
                subsite_vertex.sa
            };
            current_vertex.sa = current_vertex.sa.union(subsize);
            this.vertices.insert(site, current_vertex);
            Ok(())
        })?;

        if current_vertex.lowlink == current_vertex.index {
            while let Some(top) = self.stack.pop() {
                let top_vertex = self.vertices.get_mut(&top).unwrap();
                top_vertex.on_stack = false;
                top_vertex.sa = current_vertex.sa;
                if top == site {
                    break;
                }
            }
        }

        Ok(current_vertex)
    }

    pub fn size(&mut self, site: TailCallSite<'comp>) -> Result<TailInfo, LayoutError> {
        if let Some(&vertex) = self.vertices.get(&site) {
            return Ok(vertex.tail_info());
        }
        Ok(self.calculate_tail_size(site)?.tail_info())
    }
}
