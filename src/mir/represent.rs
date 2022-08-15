use std::{fmt::Display, io::Write};

use crate::{databased_display::DatabasedDisplay, dbwrite};

use super::{
    BBRef, BlockExit, CallKind, Comp, DupleField, ExceptionRetreat, Function, IntBinOp, IntUnOp,
    MirInstr, Mirs, Place, PlaceOrigin, PlaceRef, ReturnStatus, StackRef, Val, PdArgKind,
};

impl Display for StackRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl<DB: Mirs> DatabasedDisplay<(&Function, &DB)> for PlaceRef {
    fn db_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        (fun, db): &(&Function, &DB),
    ) -> std::fmt::Result {
        let place = fun.place(*self).place;
        match place {
            Place::Captures => write!(f, "%cap"),
            Place::Arg => write!(f, "%arg"),
            Place::Return => write!(f, "%ret"),
            Place::Stack(s) => s.db_fmt(f, db),
            Place::Field(inner, field) => {
                inner.db_fmt(f, &(*fun, *db))?;
                let name = db
                    .lookup_intern_hir_path(field)
                    .path()
                    .last()
                    .unwrap()
                    .unwrap_named();
                dbwrite!(f, *db, ".{}", &name)
            }
            Place::DupleField(inner, field) => {
                inner.db_fmt(f, &(*fun, *db))?;
                let s = match field {
                    DupleField::First => "0",
                    DupleField::Second => "1",
                };
                write!(f, ".{}", s)
            }
            Place::From(inner) => {
                inner.db_fmt(f, &(*fun, *db))?;
                write!(f, ".from")
            }
        }
    }
}

impl Display for IntBinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntBinOp::And => "and",
            IntBinOp::Xor => "xor",
            IntBinOp::Or => "or",
            IntBinOp::ShiftR => "shr",
            IntBinOp::ShiftL => "shl",
            IntBinOp::Minus => "sub",
            IntBinOp::Plus => "add",
            IntBinOp::Div => "div",
            IntBinOp::Modulo => "mod",
            IntBinOp::Mul => "mul",
        };
        write!(f, "{}", s)
    }
}

impl Display for IntUnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntUnOp::Not => "not",
            IntUnOp::Neg => "neg",
        };
        write!(f, "{}", s)
    }
}

impl Display for Comp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Comp::LesserEq => "leq",
            Comp::Lesser => "lt",
            Comp::GreaterEq => "geq",
            Comp::Greater => "gt",
            Comp::Uneq => "neq",
            Comp::Equals => "eq",
        };
        write!(f, "{}", s)
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Char(c) => write!(f, "'{}'", char::try_from(*c).unwrap()),
            Val::Int(i) => write!(f, "{}", i),
        }
    }
}

impl Display for ExceptionRetreat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{backtrack: {}, eof: {}, error: {}}}",
            &self.backtrack, &self.eof, &self.error
        )
    }
}

impl Display for CallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            CallKind::Len => "len",
            CallKind::Val => "val",
        };
        write!(f, "{}", s)
    }
}

impl<DB: Mirs> DatabasedDisplay<(&Function, &DB)> for MirInstr {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &(&Function, &DB)) -> std::fmt::Result {
        match self {
            MirInstr::IntBin(target, op, left, right) => {
                dbwrite!(f, db, "{} = {} {}, {}", target, op, left, right)
            }
            MirInstr::IntUn(target, op, right) => {
                dbwrite!(f, db, "{} = {} {}", target, op, right)
            }
            MirInstr::Comp(target, op, left, right) => {
                dbwrite!(f, db, "{} = {} {}, {}", target, op, left, right)
            }
            MirInstr::StoreVal(target, val) => {
                dbwrite!(f, db, "{} = load {}", target, val)
            }
            MirInstr::Call(target, kind, fun, arg, retreat) => {
                dbwrite!(
                    f,
                    db,
                    "{} = call {}.{}({}), {}",
                    target,
                    fun,
                    kind,
                    arg,
                    retreat
                )
            }
            MirInstr::Field(target, inner, field, error) => {
                dbwrite!(f, db, "{} = access_field {}.", target, inner)?;
                dbwrite!(f, db.1, "{} {{error: {}}}", field, error)
            }
            MirInstr::AssertVal(target, sub, inner) => {
                dbwrite!(f, db, "assert_val {}.", target)?;
                dbwrite!(f, db.1, "{} {{backtrack: {}}}", sub, inner)
            }
            MirInstr::SetDiscriminant(block, field, val) => {
                dbwrite!(f, db, "set_discriminant {}.", block)?;
                dbwrite!(f, db.1, "{}, {}", field, val)
            }
            MirInstr::Copy(target, origin, error) => {
                dbwrite!(f, db, "{} = copy {}, {{error: {}}}", target, origin, error)
            }
        }
    }
}

impl Display for BBRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb_{}", self.0)
    }
}

impl Display for BlockExit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BlockExit::BlockInProgress => write!(f, "block_in_progress"),
            BlockExit::Jump(bb) => write!(f, "jump {}", bb),
            BlockExit::Return(r) => write!(f, "return {}", r),
        }
    }
}

impl Display for ReturnStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ReturnStatus::Ok => "ok",
            ReturnStatus::Error => "error",
            ReturnStatus::Eof => "eof",
            ReturnStatus::Backtrack => "backtrack",
        };
        write!(f, "{}", s)
    }
}

impl<DB: Mirs + ?Sized> DatabasedDisplay<DB> for PlaceOrigin {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            PlaceOrigin::Node(n) => dbwrite!(f, db, "node {}", n),
            PlaceOrigin::Ambient(_, n) => dbwrite!(f, db, "ambient {}", n),
            PlaceOrigin::Expr(n, i) => dbwrite!(f, db, "expr {}:{}", &n.0, &i.as_usize()),
        }
    }
}

impl<DB: Mirs> DatabasedDisplay<DB> for Function {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        for (place_ref, place) in self.iter_places() {
            if let Place::Stack(st) = self.place(place_ref).place {
                dbwrite!(f, db, "// origin: {}\n", &self.stack(st))?;
            }
            write!(f, "define ")?;
            place_ref.db_fmt(f, &(self, db))?;
            dbwrite!(f, db, ": {}\n", &place.ty)?;
        }
        for (bb_ref, bb) in self.iter_bb() {
            dbwrite!(f, db, "{}:\n", &bb_ref)?;
            for ins in bb.ins.iter() {
                dbwrite!(f, &(self, db), "\t{}\n", ins)?;
            }
            write!(f, "\t{}\n\n", bb.exit)?;
        }
        Ok(())
    }
}

pub fn print_all_mir<DB: Mirs, W: Write>(db: &DB, w: &mut W) -> std::io::Result<()> {
    let convert_error_ignore =
        |_| std::io::Error::new(std::io::ErrorKind::InvalidInput, "could not get mir");
    for pd in db.all_parserdefs() {
        dbwrite!(
            w,
            db,
            "---\nmir parserdef len {}:\n",
            &db.def_name(pd.0).unwrap()
        )?;
        dbwrite!(
            w,
            db,
            "{}",
            &db.mir_pd(pd, CallKind::Len, PdArgKind::Parse)
                .map_err(convert_error_ignore)?
        )?;
        dbwrite!(
            w,
            db,
            "---\nmir parserdef val {}:\n",
            &db.def_name(pd.0).unwrap()
        )?;
        dbwrite!(
            w,
            db,
            "{}",
            &db.mir_pd(pd, CallKind::Val, PdArgKind::Thunk)
                .map_err(convert_error_ignore)?
        )?;
        for block in db.all_parserdef_blocks(pd).iter() {
            for call in [CallKind::Len, CallKind::Val] {
                dbwrite!(w, db, "---\nmir block {} {}:\n", &call, &block.0)?;
                dbwrite!(
                    w,
                    db,
                    "{}",
                    &db.mir_block(*block, call).map_err(convert_error_ignore)?
                )?;
            }
        }
    }
    Ok(())
}
