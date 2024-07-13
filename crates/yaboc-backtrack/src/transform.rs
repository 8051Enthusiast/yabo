use std::sync::Arc;

use fxhash::FxHashMap;
use yaboc_base::error::SResult;
use yaboc_types::{DefId, NominalTypeHead, Type, TypeId};

use crate::{
    matrix::{Matrix, MatrixArena, MatrixView, Row, TypeVarOccurence, VarRow},
    TypeLookup,
};

pub struct TransformInfo<'a> {
    matrix: Matrix<'a>,
    from_ty: TypeId,
    to_ty: TypeId,
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

impl TypeCache {
    pub fn row_count<Info: TypeLookup + ?Sized>(&mut self, ty: TypeId, db: &Info) -> SResult<u32> {
        if let Some(&rows) = self.rows.get(&ty) {
            return Ok(rows);
        }
        let rows = match db.lookup(ty) {
            Type::Unknown => 0,
            Type::Primitive(_) => 0,
            Type::TypeVarRef(_) => 1,
            Type::Nominal(nom) => {
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

pub trait TypeBtInfo<'a> {
    fn deref_matrix(&self, def: DefId) -> SResult<Option<TransformInfo>>;
    fn field(&self, id: DefId) -> SResult<TransformInfo<'a>>;
    fn parserdef(&self, id: DefId) -> SResult<(Matrix<'a>, TypeId)>;
    type Lookup: TypeLookup;
    fn types(&self) -> &Self::Lookup;
}

impl<'a, T: TypeBtInfo<'a>> TypeLookup for T {
    fn lookup(&self, ty: TypeId) -> Type {
        self.types().lookup(ty)
    }

    fn bound_types(&self, id: DefId) -> SResult<Arc<[TypeId]>> {
        self.types().bound_types(id)
    }

    fn subst_ty(&self, ty: TypeId, subst: Arc<Vec<TypeId>>) -> TypeId {
        self.types().subst_ty(ty, subst)
    }

    fn least_deref_type(&self, ty: TypeId) -> SResult<TypeId> {
        self.types().least_deref_type(ty)
    }
}

pub struct TypeMatrixCtx<'a, Info: TypeBtInfo<'a>> {
    rows: TypeCache,
    derefs: FxHashMap<(TypeId, TypeId), Matrix<'a>>,
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

impl<'short, 'arena: 'short, Info: TypeBtInfo<'arena>> TypeMatrixCtx<'arena, Info> {
    pub fn new(db: &'arena Info, arena: MatrixArena<'arena>) -> Self {
        Self {
            rows: TypeCache::default(),
            derefs: FxHashMap::default(),
            db,
            arena,
        }
    }

    pub fn row_count(&mut self, ty: TypeId) -> SResult<u32> {
        self.rows.row_count(ty, self.db)
    }

    fn transform_args(
        &mut self,
        matrix: MatrixView<'short>,
        from_ty: TypeId,
        to_ty: TypeId,
        arg_col: u32,
    ) -> SResult<(MatrixView<'short>, u32)> {
        let [from, to] = [from_ty, to_ty].map(|ty| self.db.lookup(ty));
        // note that the patterns are reversed here because of contravariance
        let [Some((_, to_tys)), Some((_, from_tys))] = [&from, &to].map(fun_ty_parts) else {
            panic!("Expected function type, got {:?} and {:?}", &from, &to);
        };
        assert_eq!(from_tys.len(), to_tys.len());
        let mut current_col = arg_col;
        let subview = matrix.subview_from(1);
        let mut new_subview = subview;
        for (from_ty, to_ty) in from_tys.iter().zip(to_tys.iter()) {
            let col_count = self.row_count(*from_ty)?;
            if from_ty == to_ty {
                current_col += col_count;
                continue;
            }
            let transform = self.deref_nom_flattened(*from_ty, *to_ty)?;
            new_subview =
                self.arena
                    .multiply_lhs_view_column_subrange(new_subview, transform, current_col);
            current_col += col_count;
        }
        Ok((
            matrix.transform_with_subview(subview, new_subview),
            current_col,
        ))
    }

    fn transform_impl(
        &mut self,
        matrix: MatrixView<'short>,
        from_ty: TypeId,
        to_ty: TypeId,
        col: u32,
        arg_col: u32,
    ) -> SResult<MatrixView<'short>> {
        if from_ty == to_ty {
            return Ok(matrix);
        }
        let [from_type, to_type] = [from_ty, to_ty].map(|ty| self.db.lookup(ty));
        Ok(match [&from_type, &to_type] {
            [_, Type::Unknown] => {
                let empty_matrix = self.arena.new_matrix(None);
                self.arena.replace_view_content(matrix, empty_matrix)
            }
            [Type::Unknown, _] => {
                let empty_row = Row::Vars(VarRow::empty(col, arg_col));
                let row_num = self.row_count(to_ty)?;
                let replacement = self
                    .arena
                    .new_matrix(std::iter::repeat(empty_row).take(row_num as usize));
                self.arena.replace_view_content(matrix, replacement)
            }
            [Type::Nominal(..), _] => {
                let transform = self.deref_nom(from_ty, to_ty)?;
                self.arena.multiply_rhs_view(transform, matrix)
            }
            [Type::TypeVarRef(_), Type::TypeVarRef(_)]
            | [Type::Primitive(_), Type::Primitive(_)] => matrix,
            [Type::Loop(_, lhs_inner), Type::Loop(_, rhs_inner)] => {
                self.transform_impl(matrix, *lhs_inner, *rhs_inner, col, arg_col)?
            }
            [Type::ParserArg {
                result: lhs_result, ..
            }, Type::ParserArg {
                result: rhs_result, ..
            }]
            | [Type::FunctionArg(lhs_result, _), Type::FunctionArg(rhs_result, _)] => {
                let (subview, sub_arg_col) =
                    self.transform_args(matrix, from_ty, to_ty, arg_col)?;
                let subview = subview.subview_from(1);
                let new_subview =
                    self.transform_impl(subview, *lhs_result, *rhs_result, col, sub_arg_col)?;
                matrix.transform_with_subview(subview, new_subview)
            }
            [l, r] => {
                panic!("Mismatched types: {:?} and {:?}", &l, &r)
            }
        })
    }

    fn adjust_matching_nominal_head(
        &mut self,
        from: TypeId,
        to: TypeId,
    ) -> SResult<Matrix<'arena>> {
        let columns = self.row_count(from)?;
        let matrix = self.arena.identity(columns);
        let mut view = matrix.view(0..columns);
        let mut subview = view.with_length(0);
        let [Type::Nominal(from_head), Type::Nominal(to_head)] =
            [from, to].map(|ty| self.db.lookup(ty))
        else {
            panic!("Arguments must both be nominal types");
        };
        assert_eq!(from_head.def, to_head.def);
        let bound_types = self.db.bound_types(from_head.def)?;
        for arg in bound_types.iter() {
            let from_arg = self.db.subst_ty(*arg, from_head.ty_args.clone());
            let to_arg = self.db.subst_ty(*arg, to_head.ty_args.clone());
            subview = subview.next_subview(self.row_count(from_arg)?);
            let new_subview = self.transform_impl(subview, from_arg, to_arg, 0, columns)?;
            view = view.transform_with_subview(subview, new_subview);
            subview = new_subview;
        }
        Ok(self
            .arena
            .substitute_with_true_from(view.as_matrix(), columns))
    }

    fn typevar_positions_impl(
        &mut self,
        ty: TypeId,
        offset: &mut u32,
        res: &mut Vec<TypeVarOccurence>,
    ) -> SResult<()> {
        match self.db.lookup(ty) {
            Type::Unknown => {}
            Type::Primitive(_) => {}
            Type::TypeVarRef(var_ref) => {
                let typevar_idx = var_ref.1;
                res.push(TypeVarOccurence {
                    matrix_idx: *offset,
                    typevar_idx,
                });
                *offset += 1;
            }
            Type::Nominal(nom) => {
                for arg in self.db.bound_types(nom.def)?.iter() {
                    let subst = self.db.subst_ty(*arg, nom.ty_args.clone());
                    self.typevar_positions_impl(subst, offset, res)?;
                }
            }
            Type::Loop(_, inner) => {
                self.typevar_positions_impl(inner, offset, res)?;
            }
            Type::ParserArg { result, .. } | Type::FunctionArg(result, _) => {
                *offset += 1;
                self.typevar_positions_impl(result, offset, res)?;
            }
        }
        Ok(())
    }

    fn typevar_positions(&mut self, ty: TypeId) -> SResult<Vec<TypeVarOccurence>> {
        let mut res = Vec::new();
        let mut offset = 0;
        self.typevar_positions_impl(ty, &mut offset, &mut res)?;
        Ok(res)
    }

    fn nom_transform(
        &mut self,
        from: &NominalTypeHead,
        deref: TransformInfo,
    ) -> SResult<(Matrix<'arena>, TypeId)> {
        let sizes = from
            .ty_args
            .iter()
            .map(|&ty| self.row_count(ty))
            .collect::<SResult<Vec<_>>>()?;
        let columns = self.typevar_positions(deref.from_ty)?;
        let rows = self.typevar_positions(deref.to_ty)?;
        let subst_matrix = self
            .arena
            .replace_typevar(deref.matrix, &rows, &columns, &sizes);
        let subst_to_ty = self.db.subst_ty(deref.to_ty, from.ty_args.clone());
        Ok((subst_matrix, subst_to_ty))
    }

    fn deref_matrix(
        &mut self,
        from: &NominalTypeHead,
    ) -> SResult<Option<(Matrix<'arena>, TypeId)>> {
        let deref = self.db.deref_matrix(from.def)?;
        deref
            .map(|deref| self.nom_transform(from, deref))
            .transpose()
    }

    fn deref_nom(&mut self, from_ty: TypeId, to_ty: TypeId) -> SResult<Matrix<'arena>> {
        if from_ty == to_ty {
            let row_count = self.row_count(from_ty)?;
            return Ok(self.arena.identity(row_count));
        }
        if let Some(matrix) = self.derefs.get(&(from_ty, to_ty)) {
            return Ok(*matrix);
        }
        let from_type = self.db.lookup(from_ty);
        let Type::Nominal(ref from) = from_type else {
            panic!("Expected nominal type");
        };
        if let Type::Nominal(to) = self.db.lookup(to_ty) {
            if from.def == to.def {
                let matrix = self.adjust_matching_nominal_head(from_ty, to_ty)?;
                self.derefs.insert((from_ty, to_ty), matrix);
                return Ok(matrix);
            }
        }
        let (matrix, deref_to) = self
            .deref_matrix(from)?
            .expect("Heads do not match, but from_ty is unable to get deref'ed");
        let matrix = if let Type::Nominal(_) = self.db.lookup(deref_to) {
            let post_mul_matrix = self.deref_nom(deref_to, to_ty)?;
            self.arena.multiply(post_mul_matrix, matrix)
        } else {
            let from = MatrixInfo {
                shape: MatrixShape {
                    ty: deref_to,
                    bound: self.row_count(deref_to)?,
                },
                matrix,
            };
            self.transform(from, to_ty)?
        };
        self.derefs.insert((from_ty, to_ty), matrix);
        Ok(matrix)
    }

    fn deref_nom_flattened(&mut self, from_ty: TypeId, to_ty: TypeId) -> SResult<Matrix<'arena>> {
        let deref = self.deref_nom(from_ty, to_ty)?;
        let col_count = self.row_count(from_ty)?;
        Ok(self.arena.substitute_with_true_from(deref, col_count))
    }

    pub fn transform(
        &mut self,
        from_matrix: MatrixInfo<'short>,
        to_ty: TypeId,
    ) -> SResult<Matrix<'short>> {
        let view = from_matrix
            .matrix
            .view(0..self.row_count(from_matrix.shape.ty)?);
        Ok(self
            .transform_impl(
                view,
                from_matrix.shape.ty,
                to_ty,
                from_matrix.shape.bound,
                from_matrix.shape.bound,
            )?
            .as_matrix())
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
        let transformed = self.transform(from, to_shape.ty)?;
        let res = self.change_bound(transformed, from.shape.bound, to_shape.bound);
        Ok(to_shape.with_matrix(res))
    }

    fn on_ldt<'maybe_short, T>(
        &mut self,
        non_ldt: MatrixInfo<'short>,
        target_shape: MatrixShape,
        f: impl FnOnce(&mut Self, MatrixInfo<'short>) -> SResult<(Matrix<'maybe_short>, TypeId, T)>,
    ) -> SResult<(MatrixInfo<'maybe_short>, Option<T>)>
    where
        'arena: 'maybe_short,
    {
        let ldt = self.db.least_deref_type(non_ldt.shape.ty)?;
        let ldt_matrix = self.transform_and_change_bound(non_ldt, target_shape.with_ty(ldt))?;
        let (res, res_ty, ret) = f(self, ldt_matrix)?;
        let transformed = self.transform(
            target_shape.with_ty(res_ty).with_matrix(res),
            target_shape.ty,
        )?;
        Ok((target_shape.with_matrix(transformed), Some(ret)))
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

    pub fn partial_apply(
        &mut self,
        fun: MatrixInfo<'short>,
        args: impl Iterator<Item = MatrixInfo<'short>>,
        to: MatrixShape,
    ) -> SResult<Matrix<'short>> {
        let fun_type = self.db.lookup(fun.shape.ty);
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
        let arg_matrix = self.arena.concatenate_rows(arg_matrix);
        let fun_matrix = self.change_bound(fun.matrix, fun.shape.bound, to.bound);
        Ok(self.arena.partial_apply(fun_matrix, arg_matrix, to.bound))
    }

    pub fn eval(
        &mut self,
        from: MatrixInfo<'short>,
        to: MatrixShape,
    ) -> SResult<(MatrixInfo<'short>, Option<Row>)> {
        self.on_ldt(from, to, |this, from| {
            let ldt_type = this.db.lookup(from.shape.ty);
            let Some((res, [])) = fun_ty_parts(&ldt_type) else {
                panic!("Expected function type, got {:?}", &ldt_type);
            };
            let rest_matrix = Matrix::from_rows(&from.matrix.rows()[1..]);
            let effect = from.matrix.rows()[0].clone();
            Ok((rest_matrix, res, effect))
        })
    }

    // a combination of partial_apply and eval, for parsers where we don't store the intermediate type (well, it isn't representable)
    pub fn parse(
        &mut self,
        fun: MatrixInfo<'short>,
        arg: MatrixInfo<'short>,
        to: MatrixShape,
    ) -> SResult<(MatrixInfo<'short>, Option<Row>)> {
        self.on_ldt(fun, to, |this, fun| {
            let fun_ldt_type = this.db.lookup(fun.shape.ty);
            let Some((res, [arg_ty])) = fun_ty_parts(&fun_ldt_type) else {
                panic!("Expected parser type, got {:?}", &fun_ldt_type);
            };
            let arg_matrix = this.transform_and_change_bound(arg, to.with_ty(*arg_ty))?;
            let applied = this.partial_apply(fun, Some(arg_matrix).into_iter(), to)?;
            let effect = applied.rows()[0].clone();
            let rest_matrix = Matrix::from_rows(&applied.rows()[1..]);
            Ok((rest_matrix, res, effect))
        })
    }

    pub fn identity(&mut self, shape: MatrixShape) -> SResult<Matrix<'arena>> {
        let rows = self.row_count(shape.ty)?;
        assert_eq!(rows, shape.bound);
        Ok(self.arena.identity(rows))
    }

    fn field_matrix(
        &mut self,
        block: &NominalTypeHead,
        field_id: DefId,
    ) -> SResult<(Matrix<'arena>, TypeId)> {
        let field = self.db.field(field_id)?;
        self.nom_transform(block, field)
    }

    pub fn get_field(
        &mut self,
        block: MatrixInfo,
        field_id: DefId,
        field_shape: MatrixShape,
    ) -> SResult<MatrixInfo<'arena>> {
        Ok(self
            .on_ldt(block, field_shape, |this, block| {
                let Type::Nominal(block_head) = this.db.lookup(block.shape.ty) else {
                    panic!("Expected nominal type");
                };
                let (field_matrix, field_ty) = this.field_matrix(&block_head, field_id)?;
                let field_matrix = this.arena.multiply(field_matrix, block.matrix);
                Ok((field_matrix, field_ty, ()))
            })?
            .0)
    }

    pub fn pd_matrix(&mut self, pd: DefId, to: MatrixShape) -> SResult<Matrix<'arena>> {
        let (matrix, res_ty) = self.db.parserdef(pd)?;
        let mut extra_rows = 0;
        let nom_head = match match match self.db.lookup(to.ty) {
            Type::FunctionArg(result, _) => {
                extra_rows += 1;
                self.db.lookup(result)
            }
            otherwise => otherwise,
        } {
            Type::ParserArg { result, .. } => {
                extra_rows += 1;
                self.db.lookup(result)
            }
            otherwise => otherwise,
        } {
            Type::Nominal(nom) => nom,
            otherwise => panic!("Expected nominal type, got {:?}", otherwise),
        };
        assert_eq!(nom_head.def, pd);
        let typevar_rows = self.typevar_positions(res_ty)?;
        let typevar_columns = typevar_rows
            .iter()
            .map(|occ| TypeVarOccurence {
                matrix_idx: occ.matrix_idx - extra_rows,
                ..*occ
            })
            .collect::<Vec<_>>();
        let sizes = nom_head
            .ty_args
            .iter()
            .map(|&ty| self.row_count(ty))
            .collect::<SResult<Vec<_>>>()?;
        let matrix = self
            .arena
            .replace_typevar(matrix, &typevar_rows, &typevar_columns, &sizes);
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

    pub fn array_parser_combinator(&mut self, to: MatrixShape) -> SResult<Matrix<'arena>> {
        const MATRIX_ROWS: &[Row] = &[
            Row::const_inner([false, false, false]),
            Row::const_inner([false, false, false]),
            Row::const_inner([false, true, false]),
        ];
        const MATRIX: Matrix<'static> = Matrix::from_rows(MATRIX_ROWS);
        const TYPEVAR_COLUMNS: [TypeVarOccurence; 2] = [
            TypeVarOccurence {
                // 'ret
                matrix_idx: 1,
                typevar_idx: 1,
            },
            TypeVarOccurence {
                // 'arg
                matrix_idx: 2,
                typevar_idx: 0,
            },
        ];
        const TYPEVAR_ROWS: [TypeVarOccurence; 1] = [TypeVarOccurence {
            // 'ret
            matrix_idx: 2,
            typevar_idx: 1,
        }];
        let Type::FunctionArg(res, _) = self.db.lookup(to.ty) else {
            panic!("Expected function type");
        };
        let Type::ParserArg { result, arg } = self.db.lookup(res) else {
            panic!("Expected parser type");
        };
        let sizes = [self.row_count(result)?, self.row_count(arg)?];
        let matrix = self
            .arena
            .replace_typevar(MATRIX, &TYPEVAR_ROWS, &TYPEVAR_COLUMNS, &sizes);
        Ok(self.change_bound(matrix, 0, to.bound))
    }
}
