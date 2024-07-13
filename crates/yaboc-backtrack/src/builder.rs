use yaboc_base::error::SResult;
use yaboc_types::TypeId;

use crate::{
    transform::{fun_ty_parts, TypeCache},
    ExprNode, Instruction, TypeLookup,
};

pub struct ExpressionBuilder<'a, DB: ?Sized> {
    nodes: Vec<ExprNode>,
    bound: u32,
    row: u32,
    db: &'a DB,
    cache: TypeCache,
}

impl<'a, DB: TypeLookup + ?Sized> ExpressionBuilder<'a, DB> {
    pub fn new(db: &'a DB) -> Self {
        Self {
            nodes: Vec::new(),
            bound: 0,
            row: 0,
            db,
            cache: TypeCache::default(),
        }
    }

    pub fn ty(&self, idx: u32) -> TypeId {
        self.nodes[idx as usize].ty
    }

    fn push_node(&mut self, node: ExprNode) -> u32 {
        self.nodes.push(node);
        self.nodes.len() as u32 - 1
    }

    pub fn push(&mut self, instr: Instruction, ty: TypeId) -> SResult<u32> {
        let mut rows = self.cache.row_count(ty, self.db)?;

        let mut cols = |ty| {
            let ty = self.db.lookup(ty);
            let Some((_, args)) = fun_ty_parts(&ty) else {
                return SResult::Ok(0u32);
            };
            args.iter()
                .map(|x| self.cache.row_count(*x, self.db))
                .try_fold(0, |acc, x| Ok(acc + x?))
        };

        match &instr {
            Instruction::EnterScope => {
                self.bound += cols(ty)?;
                rows = 0;
            }
            Instruction::LeaveScope(_) => self.bound -= cols(ty)?,
            Instruction::Fail => rows = 0,
            _ => (),
        }

        let node = ExprNode {
            row_range: self.row as usize..(self.row + rows) as usize,
            ty,
            bound: self.bound,
            instr,
        };
        self.row += rows;
        Ok(self.push_node(node))
    }

    pub fn build(self) -> Vec<ExprNode> {
        self.nodes
    }
}
