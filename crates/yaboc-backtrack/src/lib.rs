use std::sync::Arc;

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
    effect_stack: Vec<Row>,
    arg_stack: Vec<Arg<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EffectError {
    UnscopedEffect(usize),
    EffectOnNonEffectfulInvocation(usize),
}

pub struct EvalCtx<'a, 'r, Trans: TypeBtInfo> {
    exprs: &'r [ExprNode],
    rows: &'r mut [Row],
    trans: TypeMatrixCtx<'a, Trans>,
    cursor: Cursor<'a>,
    errors: Vec<EffectError>,
}

impl<'a, 'r, Trans: TypeBtInfo> EvalCtx<'a, 'r, Trans> {
    pub fn new(
        exprs: &'r [ExprNode],
        rows: &'r mut [Row],
        trans: TypeMatrixCtx<'a, Trans>,
    ) -> Self {
        Self {
            exprs,
            rows,
            cursor: Cursor::default(),
            trans,
            errors: Vec::new(),
        }
    }

    fn arg_matrix(&self, idx: u32) -> MatrixInfo<'a> {
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
        combine_fun: impl FnOnce(&mut [Row], &[Row]) -> T,
    ) -> SResult<T> {
        let (source_rows, target_rows) = self.rows.split_at_mut(range.start);
        let from = from_expr.info(source_rows);
        let transformed_matrix = self.trans.transform_and_change_bound(from, to)?;
        Ok(combine_fun(
            &mut target_rows[0..range.len()],
            transformed_matrix.matrix.rows(),
        ))
    }

    fn copy_impl<T>(
        &mut self,
        from: usize,
        to: usize,
        combine_fun: impl FnOnce(&mut [Row], &[Row]) -> T,
    ) -> SResult<T> {
        let from_expr = &self.exprs[from];
        let to_expr = &self.exprs[to];
        assert!(from_expr.row_range.end <= to_expr.row_range.start);

        self.transform_into(
            from_expr,
            to_expr.shape(),
            to_expr.row_range.clone(),
            combine_fun,
        )
    }

    fn copy(&mut self, from: usize, to: usize) -> SResult<()> {
        self.copy_impl(from, to, <[_]>::clone_from_slice)
    }

    fn or(&mut self, from: usize, to: usize) -> SResult<()> {
        self.copy_impl(from, to, |to_rows, from_rows| {
            for (to_row, from_row) in to_rows.iter_mut().zip(from_rows.iter()) {
                *to_row |= from_row;
            }
        })
    }

    fn get_arg(&mut self, idx: u32) -> SResult<()> {
        let from = self.arg_matrix(idx);
        let to_expr = &self.exprs[self.cursor.idx];

        let transformed_matrix = self
            .trans
            .transform_and_change_bound(from, to_expr.shape())?;
        self.rows[to_expr.row_range.clone()].clone_from_slice(transformed_matrix.matrix.rows());
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
        self.cursor
            .effect_stack
            .push(Row::Vars(VarRow::empty(total_bound, total_bound)));
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
        self.transform_into(from_expr, to_shape, result_range, <[_]>::clone_from_slice)?;
        self.rows[cursor_range.start] = self.cursor.effect_stack.pop().unwrap();
        for row in self.rows[cursor_range].iter_mut() {
            row.set_bound_bits(total_bound);
        }
        Ok(())
    }

    fn apply(&mut self, fun: u32, args: &SmallVec<[u32; 4]>) -> SResult<()> {
        let to_expr = &self.exprs[self.cursor.idx];
        let fun_expr = &self.exprs[fun as usize];
        let (source_rows, target_rows) = self.rows.split_at_mut(to_expr.row_range.start);
        let fun_info = fun_expr.info(source_rows);
        let arg_infos = args
            .iter()
            .map(|&idx| self.exprs[idx as usize].info(source_rows));
        let applied = self
            .trans
            .partial_apply(fun_info, arg_infos, to_expr.shape())?;
        //(eprintln!("Applied: {} {:?}", applied.matrix, to_expr.row_range());
        target_rows[0..to_expr.row_range.len()].clone_from_slice(applied.matrix.rows());
        Ok(())
    }

    fn add_effect(&mut self, effectful: EvalEffectKind, effect: Row) {
        if effect.is_empty() {
            return;
        }
        //eprintln!("Effectful: {}, effect: {}", effectful, effect);
        if effectful == EvalEffectKind::None {
            return self
                .errors
                .push(EffectError::EffectOnNonEffectfulInvocation(self.cursor.idx));
        }
        match self.cursor.effect_stack.last_mut() {
            Some(row) => {
                if effectful != EvalEffectKind::Silent {
                    *row |= &effect
                }
            }
            None => self
                .errors
                .push(EffectError::UnscopedEffect(self.cursor.idx)),
        }
    }

    fn eval(&mut self, effectful: EvalEffectKind, from: u32) -> SResult<()> {
        let from_expr = &self.exprs[from as usize];
        let to_expr = &self.exprs[self.cursor.idx];
        let (source_rows, target_rows) = self.rows.split_at_mut(to_expr.row_range.start);
        let (result, effect) = self
            .trans
            .eval(from_expr.info(source_rows), to_expr.shape())?;
        target_rows[0..to_expr.row_range.len()].clone_from_slice(result.matrix.rows());
        if let Some(effect) = effect {
            self.add_effect(effectful, effect);
        }
        Ok(())
    }

    fn parse(&mut self, effectful: EvalEffectKind, fun: u32, arg: u32) -> SResult<()> {
        let fun_expr = &self.exprs[fun as usize];
        let arg_expr = &self.exprs[arg as usize];
        let to_expr = &self.exprs[self.cursor.idx];
        let (source_rows, target_rows) = self.rows.split_at_mut(to_expr.row_range.start);
        let fun_info = fun_expr.info(source_rows);
        let arg_info = arg_expr.info(source_rows);
        let (result, effect) = self.trans.parse(fun_info, arg_info, to_expr.shape())?;
        target_rows[0..to_expr.row_range.len()].clone_from_slice(result.matrix.rows());
        if let Some(effect) = effect {
            self.add_effect(effectful, effect);
        }
        Ok(())
    }

    fn get_field(&mut self, block: u32, field: DefId) -> SResult<()> {
        let block_expr = &self.exprs[block as usize];
        let to_expr = &self.exprs[self.cursor.idx];
        let (source_rows, target_rows) = self.rows.split_at_mut(to_expr.row_range.start);
        let matrix = self
            .trans
            .get_field(block_expr.info(source_rows), field, to_expr.shape())?;
        target_rows[0..to_expr.row_range.len()].clone_from_slice(matrix.matrix.rows());
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
            Instruction::Copy(from) => self.copy(*from as usize, self.cursor.idx),
            Instruction::Unify(froms) => {
                if let Some(&first) = froms.first() {
                    self.copy(first as usize, self.cursor.idx)?;
                    for from in froms.iter().skip(1) {
                        self.or(*from as usize, self.cursor.idx)?;
                    }
                }
                Ok(())
            }
            Instruction::Activate(from) => {
                self.copy(*from as usize, self.cursor.idx)?;
                self.rows[self.exprs[self.cursor.idx].row_range.start] = Row::True;
                Ok(())
            }
            Instruction::Deactivate(from) => {
                self.copy(*from as usize, self.cursor.idx)?;
                let expr = &self.exprs[self.cursor.idx];
                self.rows[expr.row_range.start] = Row::Vars(VarRow::empty(expr.bound, expr.bound));
                Ok(())
            }
            Instruction::True => {
                let expr = &self.exprs[self.cursor.idx];
                assert!(expr.row_range.len() == 1);
                self.rows[expr.row_range.start] = Row::True;
                Ok(())
            }
            Instruction::False => {
                let expr = &self.exprs[self.cursor.idx];
                assert!(expr.row_range.len() == 1);
                self.rows[expr.row_range.start] = Row::Vars(VarRow::empty(expr.bound, expr.bound));
                Ok(())
            }
            Instruction::Identity => {
                let expr = &self.exprs[self.cursor.idx];
                let shape = expr.shape();
                let matrix = self.trans.identity(shape)?;
                self.rows[expr.row_range.clone()].clone_from_slice(matrix.rows());
                for row in self.rows[expr.row_range.clone()].iter_mut() {
                    row.set_bound_bits(shape.bound);
                }
                Ok(())
            }
            Instruction::Arg(idx) => self.get_arg(*idx),
            Instruction::ParserDef(pd) => {
                let expr = &self.exprs[self.cursor.idx];
                let matrix = self.trans.pd_matrix(*pd, expr.shape())?;
                self.rows[expr.row_range.clone()].clone_from_slice(matrix.rows());
                Ok(())
            }
            Instruction::Single => {
                let expr = &self.exprs[self.cursor.idx];
                let matrix = self.trans.single_parser(expr.shape())?;
                self.rows[expr.row_range.clone()].clone_from_slice(matrix.rows());
                Ok(())
            }
            Instruction::Array => {
                let expr = &self.exprs[self.cursor.idx];
                let matrix = self.trans.array_parser_combinator(expr.shape())?;
                self.rows[expr.row_range.clone()].clone_from_slice(matrix.rows());
                Ok(())
            }
            Instruction::GetField(block, field) => self.get_field(*block, *field),
            Instruction::EnterScope => self.enter_scope(),
            Instruction::LeaveScope(ret) => self.leave_scope(*ret),
            Instruction::None => Ok(()),
            Instruction::Fail => {
                self.add_effect(EvalEffectKind::Effectful, Row::True);
                Ok(())
            }
        }?;
        //let res_matrix = self.exprs[self.cursor.idx].info(self.rows);
        //eprintln!("Result:\n{}", res_matrix.matrix);
        Ok(())
    }

    pub fn infer(mut self) -> (TypeMatrixCtx<'a, Trans>, SResult<Vec<EffectError>>) {
        let mut has_err = Ok(());
        for idx in 0..self.exprs.len() {
            self.cursor.idx = idx;
            has_err = has_err.and(self.eval_expr());
        }
        (self.trans, has_err.map(|_| self.errors))
    }
}
