use std::collections::hash_map::Entry;

use fxhash::{FxHashMap, FxHashSet};
use yaboc_absint::AbstractDomain;
use yaboc_base::dbpanic;
use yaboc_hir::HirIdWrapper;
use yaboc_types::{Type, TypeId};

use crate::{
    AbsLayoutCtx, ILayout, IMonoLayout, Layout, LayoutError, MonoLayout, collect::LayoutSet,
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PublicType {
    ty: TypeId,
    thunk: bool,
}

impl PublicType {
    pub fn new_thunk(ty: TypeId) -> Self {
        PublicType { ty, thunk: true }
    }
    pub fn new_eval(ty: TypeId) -> Self {
        PublicType { ty, thunk: false }
    }
    pub fn evaluated(self) -> Self {
        PublicType {
            ty: self.ty,
            thunk: false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Publicity {
    InsidePublic,
    Public(Option<PublicType>),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct UseInfo {
    pub public_use: bool,
}

impl Publicity {
    fn internal_only(&self) -> Publicity {
        Publicity::InsidePublic
    }
    fn internal_and_ffi(&self) -> Publicity {
        match self {
            Publicity::InsidePublic => Publicity::InsidePublic,
            Publicity::Public(_) => Publicity::Public(None),
        }
    }
    fn is_public(&self) -> bool {
        matches!(self, Publicity::Public(_))
    }
    fn evaluated(&self) -> Publicity {
        match self {
            Publicity::InsidePublic | Publicity::Public(None) => *self,
            Publicity::Public(Some(ty)) => Publicity::Public(Some(PublicType {
                ty: ty.ty,
                thunk: false,
            })),
        }
    }
}

#[derive(Debug, Default)]
pub struct UseCollections<'comp> {
    pub collected: FxHashMap<PublicType, FxHashSet<IMonoLayout<'comp>>>,
    pub public_layouts: FxHashMap<IMonoLayout<'comp>, UseInfo>,
    pub used_polymorphically: LayoutSet<'comp>,
}

impl<'comp> UseCollections<'comp> {
    pub fn is_api_visible(&self, layout: IMonoLayout<'comp>) -> bool {
        if let Some(use_info) = self.public_layouts.get(&layout) {
            use_info.public_use
        } else {
            false
        }
    }

    pub fn needs_mask_method(&self, layout: IMonoLayout<'comp>) -> bool {
        self.public_layouts.contains_key(&layout)
    }

    pub fn needs_vtable(&self, layout: IMonoLayout<'comp>) -> bool {
        self.used_polymorphically.contains(&layout) || self.is_api_visible(layout)
    }

    fn insert(&mut self, ty: PublicType, layout: IMonoLayout<'comp>) -> bool {
        match self.collected.entry(ty) {
            Entry::Occupied(mut occupied_entry) => occupied_entry.get_mut().insert(layout),
            Entry::Vacant(vacant_entry) => {
                let mut new_set = FxHashSet::default();
                new_set.insert(layout);
                vacant_entry.insert(new_set);
                true
            }
        }
    }

    fn register(&mut self, layout: IMonoLayout<'comp>, publicity: Publicity) -> bool {
        match self.public_layouts.entry(layout) {
            Entry::Occupied(mut occupied_entry) => {
                let last = occupied_entry.get_mut();
                let is_new_use = !last.public_use & publicity.is_public();
                last.public_use |= publicity.is_public();
                is_new_use
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(UseInfo {
                    public_use: publicity.is_public(),
                });
                true
            }
        }
    }

    fn collect_mono(
        &mut self,
        ctx: &mut AbsLayoutCtx<'comp>,
        publicity: Publicity,
        layout: IMonoLayout<'comp>,
    ) -> Result<(), LayoutError> {
        let new_register = self.register(layout, publicity);
        let new_type_registered = if let Publicity::Public(Some(pub_ty)) = publicity {
            self.insert(pub_ty, layout)
        } else {
            false
        };

        if !new_register && !new_type_registered {
            return Ok(());
        }

        match layout.mono_layout() {
            MonoLayout::NominalParser(_, inner, _) => {
                for layout in inner {
                    self.collect(ctx, publicity.internal_only(), *layout)?;
                }
            }

            MonoLayout::Lambda(_, captures, args) => {
                for (_, arg) in captures {
                    self.collect(ctx, publicity.internal_only(), *arg)?;
                }
                for capture in args.iter() {
                    self.collect(ctx, publicity.internal_only(), *capture)?;
                }
            }

            MonoLayout::ArrayParser(Some((inner, Some((inner2, _))))) => {
                self.collect(ctx, publicity.internal_only(), *inner)?;
                self.collect(ctx, publicity.internal_only(), *inner2)?;
            }
            MonoLayout::ArrayParser(Some((inner, None)))
            | MonoLayout::ArrayFillParser(Some((inner, _)))
            | MonoLayout::IfParser(inner, _) => {
                self.collect(ctx, publicity.internal_only(), *inner)?;
            }
            MonoLayout::ArrayParser(None) | MonoLayout::ArrayFillParser(None) => {}
            MonoLayout::BlockParser(_, captures) => {
                for (_, capture) in captures.iter() {
                    self.collect(ctx, publicity.internal_only(), *capture)?;
                }
            }
            MonoLayout::Regex(_) | MonoLayout::Primitive(_) | MonoLayout::Single => {}
            MonoLayout::Ptr => {
                if publicity.is_public() {
                    let int = ctx.dcx.primitive(yaboc_types::PrimitiveType::Int);
                    let int_ty = PublicType::new_eval(ctx.db.int());
                    self.collect(ctx, Publicity::Public(Some(int_ty)), int)?;
                }
            }
            MonoLayout::Range | MonoLayout::SlicePtr => {
                if publicity.is_public() {
                    let ptr = ctx.dcx.intern(Layout::Mono(MonoLayout::Ptr));
                    let ptr_ty = PublicType::new_thunk(ctx.db.int());
                    self.collect(ctx, Publicity::Public(Some(ptr_ty)), ptr)?;
                }
            }
            MonoLayout::Nominal(_, inner, args) => {
                if let Some(inner) = inner {
                    self.collect(ctx, publicity.internal_and_ffi(), *inner)?;
                }
                for arg in args {
                    self.collect(ctx, publicity.internal_only(), *arg)?;
                }
            }
            MonoLayout::Block(bd, btree_map) => {
                let context = bd.lookup(ctx.db)?.root_context.lookup(ctx.db)?;
                let typevars = if let Publicity::Public(Some(pub_ty)) = publicity {
                    let Type::Block(block) = ctx.db.lookup_intern_type(pub_ty.ty) else {
                        dbpanic!(
                            ctx.db,
                            "layout {} unexpectedly of non-parser type {}",
                            &layout,
                            &pub_ty.ty
                        );
                    };
                    block.ty_args
                } else {
                    Default::default()
                };
                for (name, status) in context.vars.iter() {
                    let id = status.inner();
                    let inner_publicity = if let Publicity::Public(Some(_)) = publicity {
                        let field_ty = ctx.db.parser_type_at(*id)?;
                        let subst = ctx.db.substitute_typevar(field_ty, typevars.clone());
                        Publicity::Public(Some(PublicType::new_thunk(subst)))
                    } else {
                        publicity.internal_and_ffi()
                    };
                    let layout = btree_map[name];
                    self.collect(ctx, inner_publicity, layout)?;
                }
            }
            MonoLayout::Array { parser, slice } => {
                self.collect(ctx, publicity.internal_only(), *slice)?;
                self.collect(ctx, publicity.internal_only(), *parser)?;
                let result = parser.apply_arg(ctx, *slice)?;
                let result_ty = if let Publicity::Public(Some(pub_ty)) = publicity {
                    let Type::Loop(_, result_ty) = ctx.db.lookup_intern_type(pub_ty.ty) else {
                        dbpanic!(
                            ctx.db,
                            "layout {} unexpectedly of non-parser type {}",
                            &layout,
                            &pub_ty.ty
                        );
                    };
                    Publicity::Public(Some(PublicType::new_thunk(result_ty)))
                } else {
                    Publicity::InsidePublic
                };

                self.collect(ctx, result_ty, result)?;
            }
        }

        if let Publicity::Public(Some(PublicType { thunk: true, .. })) = publicity
            && new_type_registered
        {
            let (evaluated, _) = layout.inner().evaluate(ctx)?;
            let eval_publicity = publicity.evaluated();
            self.collect(ctx, eval_publicity, evaluated)?;
        }

        Ok(())
    }

    pub fn collect(
        &mut self,
        ctx: &mut AbsLayoutCtx<'comp>,
        used: Publicity,
        layout: ILayout<'comp>,
    ) -> Result<(), LayoutError> {
        let (layout, _) = layout.normalize(ctx)?;
        for mono in &layout {
            self.collect_mono(ctx, used, mono)?;
        }
        Ok(())
    }
}
