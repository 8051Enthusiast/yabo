use fxhash::{FxHashMap, FxHashSet};
use yaboc_dependents::RequirementSet;
use yaboc_mir::{CallMeta, FunKind, MirInstr, MirKind};

use crate::{
    mir_subst::function_substitute, prop::SizeAlign, AbsLayoutCtx, ILayout, IMonoLayout,
    LayoutError, MonoLayout,
};

#[derive(Default, Clone, Copy)]
struct CallSiteVertex {
    index: usize,
    lowlink: usize,
    on_stack: bool,
    sa: Option<SizeAlign>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct CallSite<'comp>(pub ILayout<'comp>, pub IMonoLayout<'comp>);

pub struct TailCollector<'comp, 'r> {
    ctx: &'r mut AbsLayoutCtx<'comp>,
    vertices: FxHashMap<CallSite<'comp>, CallSiteVertex>,
    stack: Vec<CallSite<'comp>>,
    index: usize,
}

impl<'comp, 'r> TailCollector<'comp, 'r> {
    pub fn new(ctx: &'r mut AbsLayoutCtx<'comp>) -> Self {
        Self {
            ctx,
            vertices: Default::default(),
            stack: Default::default(),
            index: 0,
        }
    }

    fn for_each_tail_callsite(
        &mut self,
        site: CallSite<'comp>,
        mut f: impl FnMut(&mut Self, CallSite<'comp>) -> Result<(), LayoutError>,
    ) -> Result<(), LayoutError> {
        let fun_kind = match site.1.mono_layout().0 {
            MonoLayout::NominalParser(pd, _, _) => FunKind::ParserDef(*pd),
            MonoLayout::BlockParser(b, _, _, _) => FunKind::Block(*b),
            _ => return Ok(()),
        };
        let fsub = function_substitute(
            fun_kind,
            MirKind::Call(RequirementSet::all()),
            site.0,
            site.1,
            self.ctx,
        )?;
        let mut already_called = FxHashSet::default();
        for instr in fsub.f.iter_bb().flat_map(|(_, bb)| bb.ins()) {
            let MirInstr::ParseCall(.., CallMeta { tail: true, .. }, arg, fun, _) = instr else {
                continue;
            };
            let fun = fsub.place(fun);
            for inner_fun in &fun {
                let inner_site = CallSite(fsub.place(arg), inner_fun);
                if already_called.insert(inner_site) {
                    f(self, inner_site)?;
                }
            }
        }
        Ok(())
    }

    fn calulate_tail_size(&mut self, site: CallSite<'comp>) -> Result<CallSiteVertex, LayoutError> {
        let mut current_vertex = CallSiteVertex {
            index: self.index,
            lowlink: self.index,
            sa: None,
            on_stack: true,
        };
        let sa = Some(site.1.inner().size_align_without_vtable(self.ctx)?);
        self.vertices.insert(site, current_vertex);
        self.index += 1;
        self.stack.push(site);

        self.for_each_tail_callsite(site, |this, subsite| {
            let subsize = if let Some(&subsite_vertex) = this.vertices.get(&subsite) {
                if subsite_vertex.on_stack {
                    current_vertex.lowlink = current_vertex.lowlink.min(subsite_vertex.index);
                }
                subsite_vertex.sa
            } else {
                let subsite_vertex = this.calulate_tail_size(subsite)?;
                current_vertex.lowlink = subsite_vertex.lowlink.min(current_vertex.index);
                subsite_vertex.sa
            };
            let subsize = union_msa(subsize, sa);
            current_vertex.sa = union_msa(current_vertex.sa, subsize);
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

    pub fn size(&mut self, site: CallSite<'comp>) -> Result<Option<SizeAlign>, LayoutError> {
        if let Some(&vertex) = self.vertices.get(&site) {
            return Ok(vertex.sa);
        }
        Ok(self.calulate_tail_size(site)?.sa)
    }
}

fn union_msa(msa1: Option<SizeAlign>, msa2: Option<SizeAlign>) -> Option<SizeAlign> {
    match (msa1, msa2) {
        (Some(sa1), Some(sa2)) => Some(sa1.union(sa2)),
        (a, None) | (None, a) => a,
    }
}
