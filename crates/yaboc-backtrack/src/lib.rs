use std::{array::from_fn, sync::Arc};

use smallvec::SmallVec;
use transform::fun_ty_parts;
use yaboc_base::error::SResult;
use yaboc_types::{DefId, Type, TypeId};

mod builder;
mod matrix;
mod represent;
mod transform;

pub use builder::ExpressionBuilder;
pub use matrix::{Matrix, MatrixArena, Row, VarRow};
pub use transform::{MatrixInfo, MatrixShape, TransformInfo, TypeBtInfo, TypeMatrixCtx};
pub type Arena = typed_arena::Arena<Row>;

pub trait TypeLookup {
    fn lookup(&self, ty: TypeId) -> Type;
    fn bound_types(&self, id: DefId) -> SResult<Arc<[TypeId]>>;
    fn subst_ty(&self, ty: TypeId, subst: Arc<Vec<TypeId>>) -> TypeId;
    fn least_deref_type(&self, ty: TypeId) -> SResult<TypeId>;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EffectSlot {
    Present,
    Forbidden,
}

const EFFECT_SLOTS: [EffectSlot; 2] = [EffectSlot::Present, EffectSlot::Forbidden];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EvalEffectKind {
    None,
    Effectful,
    Silent,
}

impl EvalEffectKind {
    pub fn from_flags(effect: bool, silent: bool) -> Self {
        match (effect, silent) {
            (false, _) => Self::None,
            (true, false) => Self::Effectful,
            (true, true) => Self::Silent,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instruction {
    Apply(u32, SmallVec<[u32; 4]>),
    Eval(EvalEffectKind, u32),
    Parse {
        effectful: EvalEffectKind,
        fun: u32,
        arg: u32,
    },
    Copy(u32),
    Unify(SmallVec<[u32; 4]>),
    EnterScope,
    LeaveScope(u32),
    Activate(u32),
    Deactivate(u32),
    Arg(u32),
    ParserDef(DefId),
    GetField(u32, DefId),
    True,
    False,
    Identity,
    Array,
    Single,
    None,
    Fail,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExprNode {
    pub row_range: std::ops::Range<usize>,
    pub ty: TypeId,
    pub bound: u32,
    pub instr: Instruction,
}

impl ExprNode {
    fn shape(&self) -> MatrixShape {
        MatrixShape {
            ty: self.ty,
            bound: self.bound,
        }
    }

    fn info<'a>(&self, rows: &'a [Row]) -> MatrixInfo<'a> {
        MatrixInfo {
            shape: self.shape(),
            matrix: Matrix::from_rows(&rows[self.row_range.clone()]),
        }
    }

    pub fn row_range(&self) -> std::ops::Range<usize> {
        self.row_range.clone()
    }
}

struct Arg<'a> {
    ty: TypeId,
    bound: u32,
    matrix: Matrix<'a>,
}

#[derive(Default)]
struct Cursor<'a> {
    idx: usize,
    effect_stack: Vec<[Row; 2]>,
    arg_stack: Vec<Arg<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EffectError {
    UnscopedEffect(usize),
    ForbiddenParametersOnArg(usize),
    DisallowedBacktrackingArg(usize),
    EffectOnNonEffectfulInvocation(usize),
}

impl EffectError {
    pub fn index(&self) -> usize {
        match self {
            EffectError::UnscopedEffect(idx) => *idx,
            EffectError::ForbiddenParametersOnArg(idx) => *idx,
            EffectError::DisallowedBacktrackingArg(idx) => *idx,
            EffectError::EffectOnNonEffectfulInvocation(idx) => *idx,
        }
    }
}

pub struct EvalCtx<'arena, 'r, Trans: TypeBtInfo> {
    exprs: &'r [ExprNode],
    rows: [&'r mut [Row]; 2],
    trans: TypeMatrixCtx<'arena, Trans>,
    cursor: Cursor<'arena>,
    errors: Vec<EffectError>,
}

impl<'arena, 'r, Trans: TypeBtInfo> EvalCtx<'arena, 'r, Trans> {
    pub fn new(
        exprs: &'r [ExprNode],
        rows: [&'r mut [Row]; 2],
        trans: TypeMatrixCtx<'arena, Trans>,
    ) -> Self {
        Self {
            exprs,
            rows,
            cursor: Cursor::default(),
            trans,
            errors: Vec::new(),
        }
    }

    fn arg_matrix(&self, idx: u32) -> MatrixInfo<'arena> {
        let arg = &self.cursor.arg_stack[idx as usize];
        let shape = MatrixShape {
            ty: arg.ty,
            bound: arg.bound,
        };
        MatrixInfo {
            shape,
            matrix: arg.matrix,
        }
    }

    fn transform_into<T>(
        &mut self,
        from_expr: &ExprNode,
        to: MatrixShape,
        range: std::ops::Range<usize>,
        slot: EffectSlot,
        combine_fun: impl FnOnce(&mut [Row], &[Row]) -> T,
    ) -> SResult<T> {
        let (source_rows, target_rows) = self.rows[slot as usize].split_at_mut(range.start);
        let from = from_expr.info(source_rows);
        let transformed_matrix = self.trans.transform_and_change_bound(from, to, slot)?;
        Ok(combine_fun(
            &mut target_rows[0..range.len()],
            transformed_matrix.matrix.rows(),
        ))
    }

    fn copy_impl<T>(
        &mut self,
        from: usize,
        to: usize,
        slot: EffectSlot,
        combine_fun: impl FnOnce(&mut [Row], &[Row]) -> T,
    ) -> SResult<T> {
        let from_expr = &self.exprs[from];
        let to_expr = &self.exprs[to];
        assert!(from_expr.row_range.end <= to_expr.row_range.start);

        self.transform_into(
            from_expr,
            to_expr.shape(),
            to_expr.row_range.clone(),
            slot,
            combine_fun,
        )
    }

    fn copy(&mut self, from: usize, to: usize, slot: EffectSlot) -> SResult<()> {
        self.copy_impl(from, to, slot, <[_]>::clone_from_slice)
    }

    fn copy_all_slots(&mut self, from: usize, to: usize) -> SResult<()> {
        for slot in EFFECT_SLOTS {
            self.copy_impl(from, to, slot, <[_]>::clone_from_slice)?
        }
        Ok(())
    }

    fn or(&mut self, from: usize, to: usize, slot: EffectSlot) -> SResult<()> {
        self.copy_impl(from, to, slot, |to_rows, from_rows| {
            for (to_row, from_row) in to_rows.iter_mut().zip(from_rows.iter()) {
                *to_row |= from_row;
            }
        })
    }

    fn get_arg(&mut self, idx: u32) -> SResult<()> {
        let from = self.arg_matrix(idx);
        let to_expr = &self.exprs[self.cursor.idx];

        let transformed_matrix =
            self.trans
                .transform_and_change_bound(from, to_expr.shape(), EffectSlot::Present)?;
        self.rows[EffectSlot::Present as usize][to_expr.row_range.clone()]
            .clone_from_slice(transformed_matrix.matrix.rows());
        Ok(())
    }

    fn enter_scope(&mut self) -> SResult<()> {
        let expr = &self.exprs[self.cursor.idx];
        let total_bound = expr.bound;
        let ty = self.trans.db.lookup(expr.ty);
        let Some((_, args)) = fun_ty_parts(&ty) else {
            panic!("Unexpected type in enter_scope: {:?}", ty);
        };
        let mut current_bound = self
            .exprs
            .get(self.cursor.idx.wrapping_sub(1))
            .map(|expr| expr.bound)
            .unwrap_or(0);
        self.cursor.effect_stack.push(from_fn(|_| {
            Row::Vars(VarRow::empty(total_bound, total_bound))
        }));
        for arg in args {
            let matrix = self
                .trans
                .arg_matrix(*arg, total_bound, &mut current_bound)?;
            self.cursor.arg_stack.push(Arg {
                ty: *arg,
                bound: total_bound,
                matrix,
            });
        }
        Ok(())
    }

    fn leave_scope(&mut self, from: u32) -> SResult<()> {
        let expr = &self.exprs[self.cursor.idx];
        let from_expr = &self.exprs[from as usize];
        let total_bound = expr.bound;
        let ty = self.trans.db.lookup(expr.ty);
        let Some((res, _)) = fun_ty_parts(&ty) else {
            panic!("Unexpected type in leave_scope: {:?}", ty)
        };
        let to_shape = MatrixShape {
            ty: res,
            bound: from_expr.bound,
        };
        let cursor_range = expr.row_range.clone();
        let result_range = (cursor_range.start + 1)..cursor_range.end;
        for slot in EFFECT_SLOTS {
            self.transform_into(
                from_expr,
                to_shape,
                result_range.clone(),
                slot,
                <[_]>::clone_from_slice,
            )?;
        }
        let t = self.cursor.effect_stack.pop().unwrap();
        for (rows, effect_row) in self.rows.iter_mut().zip(t) {
            rows[cursor_range.start] = effect_row;
            for row in rows[cursor_range.clone()].iter_mut() {
                row.set_bound_bits(total_bound);
            }
        }
        let forbidden = self.rows[EffectSlot::Forbidden as usize][cursor_range.start]
            .extract_bound_bits(total_bound);
        self.add_forbidden(forbidden);
        Ok(())
    }

    fn apply_impl(
        &mut self,
        fun: u32,
        args: &SmallVec<[u32; 4]>,
        slot: EffectSlot,
    ) -> SResult<&mut [Row]> {
        let to_expr = &self.exprs[self.cursor.idx];
        let fun_expr = &self.exprs[fun as usize];
        let [p, f] = &mut self.rows;
        let [(source_rows_present, target_rows_present), (source_rows_forbidden, target_rows_forbidden)] =
            [p, f].map(|rows| rows.split_at_mut(to_expr.row_range.start));
        let (source_rows, target_rows) = match slot {
            EffectSlot::Present => (&*source_rows_present, target_rows_present),
            EffectSlot::Forbidden => (&*source_rows_forbidden, target_rows_forbidden),
        };
        let fun_info = fun_expr.info(source_rows);
        let arg_infos = args
            .iter()
            .map(|&idx| self.exprs[idx as usize].info(source_rows_present));
        let applied = self
            .trans
            .partial_apply(fun_info, arg_infos, to_expr.shape(), slot)?;
        let target = &mut target_rows[0..to_expr.row_range.len()];
        target.clone_from_slice(applied.matrix.rows());
        Ok(target)
    }

    fn apply(&mut self, fun: u32, args: &SmallVec<[u32; 4]>) -> SResult<()> {
        let bound = self.exprs[self.cursor.idx].bound;
        self.apply_impl(fun, args, EffectSlot::Present)?;
        let forbidden = self.apply_impl(fun, args, EffectSlot::Forbidden)?;
        let mut forbidden_row = Row::Vars(VarRow::empty(bound, bound));
        for row in forbidden.iter_mut() {
            forbidden_row |= &row.extract_bound_bits(bound);
        }
        self.add_forbidden(forbidden_row);
        let arg_infos = args
            .iter()
            .map(|&idx| self.exprs[idx as usize].info(self.rows[EffectSlot::Forbidden as usize]));
        let to = &self.exprs[self.cursor.idx].shape();
        let fun = &self.exprs[fun as usize].shape();
        let (_, args) = self
            .trans
            .collect_args(arg_infos, *fun, *to, EffectSlot::Forbidden)?;
        if args.matrix.rows().iter().any(|x| x.is_true()) {
            self.errors
                .push(EffectError::ForbiddenParametersOnArg(self.cursor.idx));
        }
        Ok(())
    }

    fn add_forbidden(&mut self, effect: Row) {
        if effect.is_empty() {
            return;
        }
        let row @ Row::Vars(_) = effect else {
            return self
                .errors
                .push(EffectError::DisallowedBacktrackingArg(self.cursor.idx));
        };
        let Some([_, forbidden]) = self.cursor.effect_stack.last_mut() else {
            return;
        };
        *forbidden |= &row;
    }

    fn add_effect(&mut self, effectful: EvalEffectKind, effect: Row) {
        if effect.is_empty() {
            return;
        }

        if effectful == EvalEffectKind::None && effect.is_true() {
            return self
                .errors
                .push(EffectError::EffectOnNonEffectfulInvocation(self.cursor.idx));
        }
        match self.cursor.effect_stack.last_mut() {
            Some([existing_effect, existing_forbidden]) => {
                if effectful == EvalEffectKind::Effectful {
                    *existing_effect |= &effect
                }
                if effectful == EvalEffectKind::None {
                    *existing_forbidden |= &effect
                }
            }
            None => self
                .errors
                .push(EffectError::UnscopedEffect(self.cursor.idx)),
        }
    }

    fn eval_impl(&mut self, from: u32, slot: EffectSlot) -> SResult<Row> {
        let from_expr = &self.exprs[from as usize];
        let to_expr = &self.exprs[self.cursor.idx];
        let (source_rows, target_rows) =
            self.rows[slot as usize].split_at_mut(to_expr.row_range.start);
        let (result, effect) =
            self.trans
                .eval(from_expr.info(source_rows), to_expr.shape(), slot)?;
        target_rows[0..to_expr.row_range.len()].clone_from_slice(result.matrix.rows());
        Ok(effect)
    }

    fn eval(&mut self, effectful: EvalEffectKind, from: u32) -> SResult<()> {
        let effect = self.eval_impl(from, EffectSlot::Present)?;
        self.eval_impl(from, EffectSlot::Forbidden)?;
        self.add_effect(effectful, effect);
        Ok(())
    }

    fn parse_impl(&mut self, fun: u32, arg: u32, slot: EffectSlot) -> SResult<(Row, &mut [Row])> {
        let fun_expr = &self.exprs[fun as usize];
        let arg_expr = &self.exprs[arg as usize];
        let to_expr = &self.exprs[self.cursor.idx];
        let [p, f] = &mut self.rows;
        let [(source_rows_present, target_rows_present), (source_rows_forbidden, target_rows_forbidden)] =
            [p, f].map(|rows| rows.split_at_mut(to_expr.row_range.start));
        let (source_rows, target_rows) = match slot {
            EffectSlot::Present => (&*source_rows_present, target_rows_present),
            EffectSlot::Forbidden => (&*source_rows_forbidden, target_rows_forbidden),
        };
        let fun_info = fun_expr.info(source_rows);
        let arg_info = arg_expr.info(source_rows_present);
        let (result, effect) = self
            .trans
            .parse(fun_info, arg_info, to_expr.shape(), slot)?;
        let target = &mut target_rows[0..to_expr.row_range.len()];
        target.clone_from_slice(result.matrix.rows());
        Ok((effect, target))
    }

    fn parse(&mut self, effectful: EvalEffectKind, fun: u32, arg: u32) -> SResult<()> {
        let bound = self.exprs[self.cursor.idx].bound;
        let (effect, _) = self.parse_impl(fun, arg, EffectSlot::Present)?;
        let (mut forbidden, forbidden_rows) = self.parse_impl(fun, arg, EffectSlot::Forbidden)?;
        for row in forbidden_rows.iter_mut() {
            forbidden |= &row.extract_bound_bits(bound);
        }
        self.add_effect(effectful, effect);
        self.add_forbidden(forbidden);
        Ok(())
    }

    fn get_field(&mut self, block: u32, field: DefId, slot: EffectSlot) -> SResult<()> {
        let block_expr = &self.exprs[block as usize];
        let to_expr = &self.exprs[self.cursor.idx];
        let (source_rows, target_rows) =
            self.rows[slot as usize].split_at_mut(to_expr.row_range.start);
        let matrix =
            self.trans
                .get_field(block_expr.info(source_rows), field, to_expr.shape(), slot)?;
        target_rows[0..to_expr.row_range.len()].clone_from_slice(matrix.matrix.rows());
        Ok(())
    }

    fn pd(&mut self, pd: DefId, slot: EffectSlot) -> SResult<()> {
        let expr = &self.exprs[self.cursor.idx];
        let matrix = self.trans.pd_matrix(pd, expr.shape(), slot)?;
        self.rows[slot as usize][expr.row_range.clone()].clone_from_slice(matrix.rows());
        Ok(())
    }

    fn eval_expr(&mut self) -> SResult<()> {
        let expr = &self.exprs[self.cursor.idx];
        //dbeprintln!(
        //    self.trans.db.types(),
        //    "Evaluating [{}]: {}",
        //    &self.cursor.idx,
        //    expr
        //);
        match &expr.instr {
            Instruction::Apply(fun, args) => self.apply(*fun, args),
            Instruction::Eval(effectful, from) => self.eval(*effectful, *from),
            Instruction::Parse {
                effectful,
                fun,
                arg,
            } => self.parse(*effectful, *fun, *arg),
            Instruction::Copy(from) => {
                self.copy_all_slots(*from as usize, self.cursor.idx)?;
                Ok(())
            }
            Instruction::Unify(froms) => {
                for slot in EFFECT_SLOTS {
                    if let Some(&first) = froms.first() {
                        self.copy(first as usize, self.cursor.idx, slot)?;
                        for from in froms.iter().skip(1) {
                            self.or(*from as usize, self.cursor.idx, slot)?;
                        }
                    }
                }
                Ok(())
            }
            Instruction::Activate(from) => {
                self.copy_all_slots(*from as usize, self.cursor.idx)?;
                self.rows[EffectSlot::Present as usize]
                    [self.exprs[self.cursor.idx].row_range.start] = Row::True;
                Ok(())
            }
            Instruction::Deactivate(from) => {
                self.copy_all_slots(*from as usize, self.cursor.idx)?;
                let expr = &self.exprs[self.cursor.idx];
                self.rows[EffectSlot::Present as usize][expr.row_range.start] =
                    Row::Vars(VarRow::empty(expr.bound, expr.bound));
                Ok(())
            }
            Instruction::True => {
                let expr = &self.exprs[self.cursor.idx];
                assert!(expr.row_range.len() == 1);
                self.rows[EffectSlot::Present as usize][expr.row_range.start] = Row::True;
                self.rows[EffectSlot::Forbidden as usize][expr.row_range.start] =
                    Row::Vars(VarRow::empty(expr.bound, expr.bound));
                Ok(())
            }
            Instruction::False => {
                let expr = &self.exprs[self.cursor.idx];
                assert!(expr.row_range.len() == 1);
                self.rows[EffectSlot::Present as usize][expr.row_range.start] =
                    Row::Vars(VarRow::empty(expr.bound, expr.bound));
                Ok(())
            }
            Instruction::Identity => {
                let expr = &self.exprs[self.cursor.idx];
                let shape = expr.shape();
                let matrix = self.trans.identity(shape)?;
                let rows = &mut self.rows[EffectSlot::Present as usize];
                rows[expr.row_range.clone()].clone_from_slice(matrix.rows());
                for row in rows[expr.row_range.clone()].iter_mut() {
                    row.set_bound_bits(shape.bound);
                }
                Ok(())
            }
            Instruction::Arg(idx) => self.get_arg(*idx),
            Instruction::ParserDef(pd) => {
                self.pd(*pd, EffectSlot::Present)?;
                self.pd(*pd, EffectSlot::Forbidden)?;
                Ok(())
            }
            Instruction::Single => {
                let expr = &self.exprs[self.cursor.idx];
                let matrix = self.trans.single_parser(expr.shape())?;
                self.rows[EffectSlot::Present as usize][expr.row_range.clone()]
                    .clone_from_slice(matrix.rows());
                Ok(())
            }
            Instruction::Array => {
                let expr = &self.exprs[self.cursor.idx];
                let matrix = self.trans.array_parser_combinator(expr.shape())?;
                for (rows, matrix) in self.rows.iter_mut().zip(matrix.into_iter()) {
                    rows[expr.row_range.clone()].clone_from_slice(matrix.rows());
                }
                Ok(())
            }
            Instruction::GetField(block, field) => {
                self.get_field(*block, *field, EffectSlot::Present)?;
                self.get_field(*block, *field, EffectSlot::Forbidden)?;
                Ok(())
            }
            Instruction::EnterScope => self.enter_scope(),
            Instruction::LeaveScope(ret) => self.leave_scope(*ret),
            Instruction::None => Ok(()),
            Instruction::Fail => {
                self.add_effect(EvalEffectKind::Effectful, Row::True);
                Ok(())
            }
        }?;
        //for slot in EFFECT_SLOTS {
        //    let res_matrix = self.exprs[self.cursor.idx].info(self.rows[slot as usize]);
        //    eprintln!("Result {:?}:\n{}", slot, res_matrix.matrix);
        //    for row in res_matrix.matrix.rows() {
        //        if let Row::Vars(v) = row {
        //            eprintln!("{} {}", v.bound_bits(), v.total_bits());
        //        }
        //    }
        //}
        Ok(())
    }

    pub fn infer(mut self) -> (TypeMatrixCtx<'arena, Trans>, SResult<Vec<EffectError>>) {
        let mut has_err = Ok(());
        for idx in 0..self.exprs.len() {
            self.cursor.idx = idx;
            has_err = has_err.and(self.eval_expr());
        }
        (self.trans, has_err.map(|_| self.errors))
    }
}
