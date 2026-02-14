use std::sync::Arc;

use fxhash::FxHashMap;
use yaboc_base::error::SResult;
use yaboc_types::{BlockTypeHead, DefId, Type, TypeId, TypeInterner};

use crate::{
    matrix::{Matrix, MatrixArena, Rect, Row, TypeVarOccurence, VarRow},
    EffectSlot, TypeLookup,
};

pub struct TransformInfo<'a> {
    pub matrix_present: Matrix<'a>,
    pub matrix_forbidden: Matrix<'a>,
    pub to_ty: TypeId,
}

#[derive(Clone, Copy)]
pub struct MatrixShape {
    pub ty: TypeId,
    pub bound: u32,
}

impl MatrixShape {
    pub fn with_ty(self, ty: TypeId) -> Self {
        Self { ty, ..self }
    }
    pub fn with_bound(self, bound: u32) -> Self {
        Self { bound, ..self }
    }
    pub fn with_matrix(self, matrix: Matrix) -> MatrixInfo {
        MatrixInfo {
            shape: self,
            matrix,
        }
    }
}

impl MatrixShape {
    pub fn new(ty: TypeId, bound: u32) -> Self {
        Self { ty, bound }
    }
}

#[derive(Clone, Copy)]
pub struct MatrixInfo<'a> {
    pub shape: MatrixShape,
    pub matrix: Matrix<'a>,
}

impl<'a> MatrixInfo<'a> {
    pub fn new(ty: TypeId, bound: u32, matrix: Matrix<'a>) -> Self {
        Self {
            shape: MatrixShape { ty, bound },
            matrix,
        }
    }
}

#[derive(Default)]
pub struct TypeCache {
    rows: FxHashMap<TypeId, u32>,
}

pub struct DefTypeSubstitution {
    pub ty: TypeId,
    pub subst_ty: TypeId,
    pub bound: Vec<TypeId>,
    pub subst_bound: Vec<TypeId>,
}

impl TypeCache {
    pub fn row_count<Info: TypeLookup + ?Sized>(&mut self, ty: TypeId, db: &Info) -> SResult<u32> {
        if let Some(&rows) = self.rows.get(&ty) {
            return Ok(rows);
        }
        let rows = match db.lookup(ty) {
            Type::Unknown => 0,
            Type::Primitive(_) => 0,
            Type::TypeVarRef(_) => 1,
            Type::Block(nom) => {
                let mut res = 0u32;
                for arg in db.bound_types(nom.def)?.iter() {
                    let subst = db.subst_ty(*arg, nom.ty_args.clone());
                    res = res.checked_add(self.row_count(subst, db)?).unwrap();
                }
                res
            }
            Type::Loop(_, inner) => self.row_count(inner, db)?,
            Type::ParserArg { result, .. } | Type::FunctionArg(result, _) => {
                self.row_count(result, db)?.checked_add(1).unwrap()
            }
        };
        self.rows.insert(ty, rows);
        Ok(rows)
    }
}

pub trait TypeBtInfo {
    fn deref_matrix(&self, def: DefId) -> SResult<Option<TransformInfo<'_>>>;
    fn field(&self, id: DefId) -> SResult<TransformInfo<'_>>;
    fn parserdef(&self, id: DefId) -> SResult<TransformInfo<'_>>;
    fn partial_apply_ty(&self, ty: TypeId, applied_arg_count: usize) -> TypeId;
    type Lookup: TypeLookup + TypeInterner + ?Sized;
    fn types(&self) -> &Self::Lookup;
}

impl<T: TypeBtInfo> TypeLookup for T {
    fn lookup(&self, ty: TypeId) -> Type {
        self.types().lookup(ty)
    }

    fn bound_types(&self, id: DefId) -> SResult<Arc<[TypeId]>> {
        self.types().bound_types(id)
    }

    fn subst_ty(&self, ty: TypeId, subst: Arc<Vec<TypeId>>) -> TypeId {
        self.types().subst_ty(ty, subst)
    }
}

pub struct TypeMatrixCtx<'a, Info: TypeBtInfo> {
    rows: TypeCache,
    pub db: &'a Info,
    arena: MatrixArena<'a>,
}

pub fn fun_ty_parts(ty: &Type) -> Option<(TypeId, &[TypeId])> {
    match ty {
        Type::FunctionArg(result, args) => Some((*result, &args[..])),
        Type::ParserArg { result, arg } => Some((*result, std::slice::from_ref(arg))),
        _ => None,
    }
}

impl<'short, 'arena: 'short, Info: TypeBtInfo> TypeMatrixCtx<'arena, Info> {
    pub fn new(db: &'arena Info, arena: MatrixArena<'arena>) -> Self {
        Self {
            rows: TypeCache::default(),
            db,
            arena,
        }
    }

    pub fn row_count(&mut self, ty: TypeId) -> SResult<u32> {
        self.rows.row_count(ty, self.db)
    }

    fn typevar_positions_impl(
        &mut self,
        orig: TypeId,
        subst: TypeId,
        offset: &mut u32,
        res: &mut Vec<TypeVarOccurence>,
    ) -> SResult<()> {
        match [self.db.lookup(orig), self.db.lookup(subst)] {
            [Type::Unknown, Type::Unknown] => {}
            [Type::Primitive(_), Type::Primitive(_)] => {}
            [Type::TypeVarRef(_), _] => {
                let rows = self.row_count(subst)?;
                res.push(TypeVarOccurence {
                    matrix_idx: *offset,
                    substituted_width: rows,
                });
                *offset += 1;
            }
            [Type::Block(nom), Type::Block(subst_nom)] => {
                assert_eq!(nom.def, subst_nom.def);
                for arg in self.db.bound_types(nom.def)?.iter() {
                    let subst = self.db.subst_ty(*arg, subst_nom.ty_args.clone());
                    self.typevar_positions_impl(*arg, subst, offset, res)?;
                }
            }
            [Type::Loop(_, inner), Type::Loop(_, subst_inner)] => {
                self.typevar_positions_impl(inner, subst_inner, offset, res)?;
            }
            [Type::ParserArg { result, .. }, Type::ParserArg {
                result: subst_result,
                ..
            }]
            | [Type::FunctionArg(result, _), Type::FunctionArg(subst_result, _)] => {
                *offset += 1;
                self.typevar_positions_impl(result, subst_result, offset, res)?;
            }
            [orig, subst] => panic!("Type mismatch: {orig:?} and {subst:?}"),
        }
        Ok(())
    }

    fn typevar_positions(
        &mut self,
        tys: &[TypeId],
        subst_tys: &[TypeId],
    ) -> SResult<Vec<TypeVarOccurence>> {
        let mut res = Vec::new();
        let mut offset = 0;
        for (ty, subst_ty) in tys.iter().zip(subst_tys) {
            self.typevar_positions_impl(*ty, *subst_ty, &mut offset, &mut res)?;
        }
        Ok(res)
    }

    fn nom_subst_types(
        &mut self,
        pd: DefId,
        general_to: TypeId,
        to: TypeId,
    ) -> SResult<DefTypeSubstitution> {
        let bound = self.db.bound_types(pd)?.to_vec();
        let mut subst_bound = vec![];
        let mut result_ty = to;
        loop {
            match self.db.lookup(result_ty) {
                Type::ParserArg { arg, result } => {
                    subst_bound.push(arg);
                    result_ty = result;
                }
                Type::FunctionArg(result, args) => {
                    subst_bound.extend_from_slice(args.as_slice());
                    result_ty = result;
                }
                _ => break,
            }
        }
        Ok(DefTypeSubstitution {
            ty: general_to,
            subst_ty: to,
            bound: bound,
            subst_bound,
        })
    }

    fn block_subst_types(
        &mut self,
        block: &BlockTypeHead,
        to: TypeId,
    ) -> SResult<DefTypeSubstitution> {
        let bound = self.db.bound_types(block.def)?.to_vec();
        let subst_bound = bound
            .iter()
            .map(|ty| self.db.subst_ty(*ty, block.ty_args.clone()))
            .collect::<Vec<_>>();
        let subst_to = self.db.subst_ty(to, block.ty_args.clone());
        Ok(DefTypeSubstitution {
            ty: to,
            subst_ty: subst_to,
            bound: bound,
            subst_bound,
        })
    }
    fn nom_transform(
        &mut self,
        sub: &DefTypeSubstitution,
        deref_matrix: Matrix,
    ) -> SResult<Matrix<'arena>> {
        let columns = self.typevar_positions(&sub.bound, &sub.subst_bound)?;
        let rows = self.typevar_positions(&[sub.ty], &[sub.subst_ty])?;
        let subst_matrix = self.arena.replace_typevar(deref_matrix, &rows, &columns);
        Ok(subst_matrix)
    }

    pub fn change_bound(
        &mut self,
        matrix: Matrix<'short>,
        from_bound: u32,
        to_bound: u32,
    ) -> Matrix<'short> {
        if from_bound == to_bound {
            return matrix;
        }
        assert!(from_bound < to_bound);
        self.arena.add_bound_vars(matrix, to_bound - from_bound)
    }

    pub fn transform_and_change_bound(
        &mut self,
        from: MatrixInfo<'short>,
        to_shape: MatrixShape,
    ) -> SResult<MatrixInfo<'short>> {
        let res = self.change_bound(from.matrix, from.shape.bound, to_shape.bound);
        Ok(to_shape.with_matrix(res))
    }

    pub fn arg_matrix(
        &mut self,
        arg_ty: TypeId,
        total_bound: u32,
        current_bound: &mut u32,
    ) -> SResult<Matrix<'arena>> {
        let rows = self.row_count(arg_ty)?;
        let matrix = self.arena.new_matrix((0..rows).map(|row_num: u32| {
            let mut row = Row::Vars(VarRow::empty(total_bound, total_bound));
            row.set(*current_bound + row_num);
            row
        }));
        *current_bound += rows;
        Ok(matrix)
    }

    pub fn collect_args(
        &mut self,
        args: impl Iterator<Item = MatrixInfo<'short>>,
        fun: MatrixShape,
        to: MatrixShape,
    ) -> SResult<(usize, Rect<Matrix<'arena>>)> {
        let fun_type = self.db.lookup(fun.ty);
        let (_, fun_args) = fun_ty_parts(&fun_type).expect("Expected function type");
        let arg_matrix = fun_args
            .iter()
            .zip(args)
            .map(|(to_ty, from)| {
                Ok(self
                    .transform_and_change_bound(from, MatrixShape::new(*to_ty, to.bound))?
                    .matrix)
            })
            .collect::<SResult<Vec<Matrix>>>()?;
        let arg_count = arg_matrix.len();
        let arg_matrix = self.arena.concatenate_rows(arg_matrix);
        let arg_matrix = self
            .arena
            .substitute_with_true_from(arg_matrix, to.bound)
            .rect(to.bound, to.bound);
        Ok((arg_count, arg_matrix))
    }

    fn partial_apply_impl(
        &mut self,
        fun: MatrixInfo<'short>,
        args: impl Iterator<Item = MatrixInfo<'short>>,
        to: MatrixShape,
    ) -> SResult<(Matrix<'short>, usize)> {
        let (arg_count, arg_matrix) = self.collect_args(args, fun.shape, to)?;
        let fun_matrix = self.change_bound(fun.matrix, fun.shape.bound, to.bound);
        let result = self.arena.partial_apply(fun_matrix, arg_matrix, to.bound);
        Ok((result, arg_count))
    }

    pub fn partial_apply(
        &mut self,
        fun: MatrixInfo<'short>,
        args: impl Iterator<Item = MatrixInfo<'short>>,
        to: MatrixShape,
    ) -> SResult<MatrixInfo<'short>> {
        let (res, _) = self.partial_apply_impl(fun, args, to)?;
        Ok(to.with_matrix(res))
    }

    pub fn eval(
        &mut self,
        from: MatrixInfo<'short>,
        to: MatrixShape,
    ) -> SResult<(MatrixInfo<'short>, Row)> {
        let rest_matrix = Matrix::from_rows(&from.matrix.rows()[1..]);
        let effect = from.matrix.rows()[0].clone();
        Ok((to.with_matrix(rest_matrix), effect))
    }

    // a combination of partial_apply and eval, for parsers where we don't store the intermediate type (well, it isn't representable)
    pub fn parse(
        &mut self,
        fun: MatrixInfo<'short>,
        arg: MatrixInfo<'short>,
        to: MatrixShape,
    ) -> SResult<(MatrixInfo<'short>, Row)> {
        let fun_ldt_type = self.db.lookup(fun.shape.ty);
        let Some((_, [arg_ty])) = fun_ty_parts(&fun_ldt_type) else {
            panic!("Expected parser type, got {:?}", &fun_ldt_type);
        };
        let arg_matrix = self.transform_and_change_bound(arg, to.with_ty(*arg_ty))?;
        let applied = self
            .partial_apply_impl(fun, Some(arg_matrix).into_iter(), to)?
            .0;
        let effect = applied.rows()[0].clone();
        let rest_matrix = Matrix::from_rows(&applied.rows()[1..]);
        Ok((to.with_matrix(rest_matrix), effect))
    }

    pub fn identity(&mut self, shape: MatrixShape) -> SResult<Matrix<'arena>> {
        let rows = self.row_count(shape.ty)?;
        assert_eq!(rows, shape.bound);
        Ok(self.arena.identity(rows))
    }

    fn field_matrix(
        &mut self,
        block: &BlockTypeHead,
        field_id: DefId,
        slot: EffectSlot,
    ) -> SResult<(Matrix<'arena>, TypeId)> {
        let field = self.db.field(field_id)?;
        let matrix = match slot {
            EffectSlot::Present => field.matrix_present,
            EffectSlot::Forbidden => field.matrix_forbidden,
        };
        let subst = self.block_subst_types(block, field.to_ty)?;
        Ok((self.nom_transform(&subst, matrix)?, subst.subst_ty))
    }

    pub fn get_field(
        &mut self,
        block: MatrixInfo,
        field_id: DefId,
        field_shape: MatrixShape,
        slot: EffectSlot,
    ) -> SResult<MatrixInfo<'arena>> {
        let Type::Block(block_head) = self.db.lookup(block.shape.ty) else {
            panic!("Expected nominal type");
        };
        let (field_matrix, _) = self.field_matrix(&block_head, field_id, slot)?;
        let rect_matrix = block.matrix.rect(block.shape.bound, block.shape.bound);
        let field_matrix = self.arena.multiply(field_matrix, rect_matrix);
        Ok(field_shape.with_matrix(field_matrix))
    }

    pub fn pd_matrix(
        &mut self,
        pd: DefId,
        to: MatrixShape,
        slot: EffectSlot,
    ) -> SResult<Matrix<'arena>> {
        let trans = self.db.parserdef(pd)?;
        let nom_sub = self.nom_subst_types(pd, trans.to_ty, to.ty)?;
        let matrix = match slot {
            EffectSlot::Present => trans.matrix_present,
            EffectSlot::Forbidden => trans.matrix_forbidden,
        };
        let matrix = self.nom_transform(&nom_sub, matrix)?;
        Ok(self.change_bound(matrix, 0, to.bound))
    }

    pub fn single_parser(&mut self, to: MatrixShape) -> SResult<Matrix<'arena>> {
        let bound = to.bound;
        let Type::ParserArg { result, .. } = self.db.lookup(to.ty) else {
            panic!("Expected parser type");
        };
        let res_size = self.row_count(result)?;
        Ok(self.arena.new_matrix(
            Some(Row::Vars(VarRow::empty(bound, bound + res_size)))
                .into_iter()
                .chain((0..res_size).map(|row_num| {
                    let mut row = Row::Vars(VarRow::empty(bound, bound + res_size));
                    row.set(bound + row_num);
                    row
                })),
        ))
    }

    pub fn array_parser_combinator(&mut self, to: MatrixShape) -> SResult<[Matrix<'arena>; 2]> {
        let Type::FunctionArg(res, _) = self.db.lookup(to.ty) else {
            panic!("Expected function type");
        };
        let Type::ParserArg { result, arg } = self.db.lookup(res) else {
            panic!("Expected parser type");
        };
        let sizes = [self.row_count(arg)?, self.row_count(result)?];
        // remember that the type of `array`` is (['arg] ~> 'ret, int) -> ['arg] ~> ['ret]
        const MATRIX_ROWS_PRESENT: &[Row] = &[
            //                 parse  'ret   'arg
            Row::const_inner([false, false, false]), // fun
            Row::const_inner([false, false, false]), // parse
            Row::const_inner([false, true, false]),  // 'ret
        ];
        const MATRIX_PRESENT: Matrix = Matrix::from_rows(MATRIX_ROWS_PRESENT);
        const MATRIX_ROWS_FORBIDDEN: &[Row] = &[
            //                 parse  'ret   'arg
            Row::const_inner([true, false, false]),  // fun
            Row::const_inner([false, false, false]), // parse
            Row::const_inner([false, false, false]), // 'ret
        ];
        const MATRIX_FORBIDDEN: Matrix = Matrix::from_rows(MATRIX_ROWS_FORBIDDEN);
        let typevar_columns: [TypeVarOccurence; 2] = [
            TypeVarOccurence {
                // 'ret
                matrix_idx: 1,
                substituted_width: sizes[1],
            },
            TypeVarOccurence {
                // 'arg
                matrix_idx: 2,
                substituted_width: sizes[0],
            },
        ];
        let typevar_rows: [TypeVarOccurence; 1] = [TypeVarOccurence {
            // 'ret
            matrix_idx: 2,
            substituted_width: sizes[1],
        }];
        let matrix_present =
            self.arena
                .replace_typevar(MATRIX_PRESENT, &typevar_rows, &typevar_rows);
        let matrix_present = self.change_bound(matrix_present, 0, to.bound);
        let matrix_forbidden =
            self.arena
                .replace_typevar(MATRIX_FORBIDDEN, &typevar_rows, &typevar_columns);
        let matrix_forbidden = self.change_bound(matrix_forbidden, 0, to.bound);
        Ok([matrix_present, matrix_forbidden])
    }
}
