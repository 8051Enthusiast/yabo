use yaboc_base::{
    interner::{DefId, DefinitionPath, FieldName, IdentifierName, PathComponent},
    source::FileId,
};
use yaboc_hir::{HirIdWrapper, HirNode, HirNodeKind};

use crate::{NominalId, TyHirs};

/// a convenience cursor for navigating the hir tree
/// for testing purposes
pub struct IdCursor<'db, DB: ?Sized + TyHirs> {
    db: &'db DB,
    pub id: DefId,
    pub kind: HirNodeKind,
}

impl<DB: ?Sized + TyHirs> Clone for IdCursor<'_, DB> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<DB: ?Sized + TyHirs> Copy for IdCursor<'_, DB> {}

impl<'db, DB: ?Sized + TyHirs> IdCursor<'db, DB> {
    pub fn new(db: &'db DB, id: DefId) -> Self {
        let kind = db.hir_node(id).unwrap().kind();
        Self { db, id, kind }
    }

    pub fn at_file(db: &'db DB, file: FileId) -> Self {
        let id = db.intern_hir_path(DefinitionPath::Module(file));
        if db.all_files().contains(&file) {
            Self {
                db,
                id,
                kind: HirNodeKind::Module,
            }
        } else {
            panic!("File {file:?} not in database")
        }
    }

    pub fn named_child(self, name: &str) -> Self {
        let name = self
            .db
            .intern_identifier(IdentifierName { name: name.into() });
        let field = FieldName::Ident(name);
        let child = self.id.child_field(self.db, field);
        let kind = self.db.hir_node(child).unwrap().kind();
        Self {
            db: self.db,
            id: child,
            kind,
        }
    }

    pub fn unnamed_child(self, idx: u32) -> Self {
        let child = self.id.child(self.db, PathComponent::Unnamed(idx));
        let kind = self.db.hir_node(child).unwrap().kind();
        Self {
            db: self.db,
            id: child,
            kind,
        }
    }

    pub fn pd(self, name: &str) -> Self {
        assert!(self.kind == HirNodeKind::Module);
        let ret = self.named_child(name);
        assert!(ret.kind == HirNodeKind::ParserDef);
        ret
    }

    pub fn return_block(self) -> Self {
        let ty = match self.db.hir_node(self.id).unwrap() {
            HirNode::Let(_) | HirNode::Parse(_) | HirNode::ChoiceIndirection(_) => {
                self.db.parser_type_at(self.id).unwrap()
            }
            HirNode::Expr(e) => *self.db.parser_expr_at(e.id).unwrap().root_data(),
            HirNode::ParserDef(pd) => self.db.parser_returns(pd.id).unwrap().deref,
            _ => panic!("attempt to get return block of non-parser node"),
        };
        let block = match self.db.lookup_intern_type(ty) {
            yaboc_types::Type::Nominal(n) => match NominalId::from_nominal_head(&n) {
                NominalId::Block(b) => b,
                NominalId::Def(_) => panic!(),
            },
            _ => panic!(),
        };
        Self {
            db: self.db,
            id: block.0,
            kind: HirNodeKind::Block,
        }
    }

    pub fn field(self, name: &str) -> Self {
        let ctx = match self.db.hir_node(self.id).unwrap() {
            HirNode::Block(block) => block.root_context.lookup(self.db).unwrap(),
            HirNode::Context(ctx) => ctx,
            _ => panic!(),
        };
        let ident = self
            .db
            .intern_identifier(IdentifierName { name: name.into() });
        let child = *ctx
            .vars
            .set
            .get(&FieldName::Ident(ident))
            .expect("no such field")
            .inner();
        let kind = self.db.hir_node(child).unwrap().kind();

        Self {
            db: self.db,
            id: child,
            kind,
        }
    }

    pub fn return_field(self) -> Self {
        let ctx = match self.db.hir_node(self.id).unwrap() {
            HirNode::Block(block) => block.root_context.lookup(self.db).unwrap(),
            HirNode::Context(ctx) => ctx,
            _ => panic!(),
        };
        let child = *ctx
            .vars
            .set
            .get(&FieldName::Return)
            .expect("no return field")
            .inner();
        let kind = self.db.hir_node(child).unwrap().kind();

        Self {
            db: self.db,
            id: child,
            kind,
        }
    }

    pub fn expr(self) -> Self {
        let expr = match self.db.hir_node(self.id).unwrap() {
            HirNode::Let(l) => l.expr,
            HirNode::Parse(p) => p.expr,
            HirNode::ParserDef(pd) => pd.to,
            _ => panic!("attempt to get expr of non-expr-containing node"),
        };
        let kind = HirNodeKind::Expr;
        Self {
            db: self.db,
            id: expr.0,
            kind,
        }
    }

    pub fn expr_block(self, idx: usize) -> Self {
        let HirNode::Expr(expr) = self.db.hir_node(self.id).unwrap() else {
            panic!("not an expr")
        };
        let child = *expr
            .children
            .iter()
            .filter(|c| self.db.hir_node(**c).unwrap().kind() == HirNodeKind::Block)
            .nth(idx)
            .expect("no such block");
        let kind = self.db.hir_node(child).unwrap().kind();
        Self {
            db: self.db,
            id: child,
            kind,
        }
    }

    pub fn choice(self, idx: u32) -> Self {
        let HirNode::ChoiceIndirection(ci) = self.db.hir_node(self.id).unwrap() else {
            panic!("not a choice indirection")
        };
        let child = ci
            .choices
            .iter()
            .find_map(|(i, c)| idx.eq(i).then_some(*c))
            .expect("no such choice");
        let kind = self.db.hir_node(child).unwrap().kind();
        Self {
            db: self.db,
            id: child,
            kind,
        }
    }
}
