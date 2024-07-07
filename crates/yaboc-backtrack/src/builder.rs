use yaboc_types::TypeId;

use crate::{
    transform::{fun_ty_parts, TypeCache},
    ExprNode, Instruction, TypeLookup,
};

pub struct ExpressionBuilder<'a, DB: TypeLookup> {
    nodes: Vec<ExprNode>,
    bound: u32,
    row: u32,
    db: &'a DB,
    cache: TypeCache,
}

impl<'a, DB: TypeLookup> ExpressionBuilder<'a, DB> {
    pub fn new(db: &'a DB) -> Self {
        Self {
            nodes: Vec::new(),
            bound: 0,
            row: 0,
            db,
            cache: TypeCache::default(),
        }
    }

    fn push_node(&mut self, node: ExprNode) -> u32 {
        self.nodes.push(node);
        self.nodes.len() as u32 - 1
    }

    pub fn push(&mut self, instr: Instruction, ty: TypeId) -> u32 {
        let mut rows = self.cache.row_count(ty, self.db);

        let mut cols = |ty| {
            let ty = self.db.lookup(ty);
            let Some((_, args)) = fun_ty_parts(&ty) else {
                return 0u32;
            };
            args.iter().map(|x| self.cache.row_count(*x, self.db)).sum()
        };

        if let Instruction::EnterScope = &instr {
            self.bound += cols(ty);
            rows = 0;
        } else if let Instruction::LeaveScope(_) = &instr {
            self.bound -= cols(ty);
        }

        let node = ExprNode {
            row_range: self.row as usize..(self.row + rows) as usize,
            ty,
            bound: self.bound,
            instr,
        };
        self.row += rows;
        self.push_node(node)
    }

    pub fn build(self) -> Vec<ExprNode> {
        self.nodes
    }
}
