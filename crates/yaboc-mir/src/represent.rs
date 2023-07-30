use std::{fmt::Display, io::Write};

use yaboc_base::{databased_display::DatabasedDisplay, dbformat, dbwrite};
use yaboc_dependents::RequirementSet;

use crate::{strictness::Strictness, CallMeta, ControlFlow, FunKind, InsRef, MirKind};

use super::{
    BBRef, Comp, ExceptionRetreat, Function, IntBinOp, IntUnOp, MirInstr, Mirs, Place, PlaceOrigin,
    PlaceRef, ReturnStatus, StackRef, Val,
};

impl Display for StackRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl<DB: Mirs + ?Sized> DatabasedDisplay<(&Function, &DB)> for PlaceRef {
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
            Place::ReturnLen => write!(f, "%retlen"),
            Place::Stack(s) => s.db_fmt(f, db),
            Place::Field(inner, field) => {
                inner.db_fmt(f, &(*fun, *db))?;
                let name = field.unwrap_name(*db);
                dbwrite!(f, *db, ".{}", &name)
            }
            Place::Captured(inner, field) => {
                inner.db_fmt(f, &(*fun, *db))?;
                dbwrite!(f, *db, ".cap[{}]", &field)
            }
            Place::Front(inner) => {
                inner.db_fmt(f, &(*fun, *db))?;
                write!(f, ".from")
            }
            Place::ModifiedBy(ins_ref) => {
                write!(f, "mod[{ins_ref}]")
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
        write!(f, "{s}")
    }
}

impl Display for IntUnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntUnOp::Not => "not",
            IntUnOp::Neg => "neg",
        };
        write!(f, "{s}")
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
        write!(f, "{s}")
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Char(c) => write!(f, "'{}'", char::try_from(*c).unwrap()),
            Val::Int(i) => write!(f, "{i}"),
            Val::Bool(b) => write!(f, "{b}"),
            Val::Undefined => write!(f, "undefined"),
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

impl Display for CallMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.req, self.tail)
    }
}

impl<DB: Mirs + ?Sized> DatabasedDisplay<(&Function, &DB)> for MirInstr {
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
            MirInstr::ParseCall(ret, retlen, kind, arg, fun, retreat) => {
                if let Some(ret) = ret {
                    dbwrite!(f, db, "{}, ", ret)?;
                } else {
                    write!(f, "_, ")?;
                }
                if let Some(retlen) = retlen {
                    dbwrite!(f, db, "{} = ", retlen)?;
                } else {
                    write!(f, "_ = ")?;
                }
                if kind.tail {
                    write!(f, "tail ")?;
                }
                dbwrite!(f, db, "parse {}({}), {}", fun, arg, &kind.req)?;
                if let Some(retreat) = retreat {
                    dbwrite!(f, db.1, ", {}", retreat)
                } else {
                    Ok(())
                }
            }
            MirInstr::LenCall(ret, fun, retreat) => {
                dbwrite!(f, db, "{} = len {}, {}", ret, fun, retreat)
            }
            MirInstr::Field(target, inner, field, cont) => {
                dbwrite!(f, db, "{} = access_field {}.", target, inner)?;
                dbwrite!(f, db.1, "{}, {}", field, cont)
            }
            MirInstr::AssertVal(target, sub, cont) => {
                dbwrite!(f, db, "assert_val {}.", target)?;
                dbwrite!(f, db.1, "{}, {}", sub, cont)
            }
            MirInstr::SetDiscriminant(block, field, val) => {
                dbwrite!(f, db, "set_discriminant {}.", block)?;
                dbwrite!(f, db.1, "{}, {}", field, val)
            }
            MirInstr::Copy(target, origin, cont) => {
                dbwrite!(f, db, "{} = copy {}, {}", target, origin, cont)
            }
            MirInstr::ApplyArgs(target, origin, args, arg_start, cont) => {
                dbwrite!(f, db, "{} = apply_args {}, (", target, origin)?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    dbwrite!(f, db, "{}", arg)?;
                }
                dbwrite!(f, db, "), {}, {}", arg_start, cont)
            }
            MirInstr::Branch(next) => {
                dbwrite!(f, db, "branch {{ next: {} }}", next)
            }
            MirInstr::Return(status) => {
                dbwrite!(f, db, "return {}", status)
            }
        }
    }
}

impl Display for BBRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb_{}", self.0)
    }
}

impl Display for InsRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let InsRef(bb, offset) = self;
        write!(f, "bb_{}[{}]", bb.0, offset)
    }
}

impl Display for ControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ next: {}", self.next)?;
        if let Some(error) = self.error {
            write!(f, ", error: {error}")?;
        }
        if let Some(eof) = self.eof {
            write!(f, ", eof: {eof}")?;
        }
        if let Some(backtrack) = self.backtrack {
            write!(f, ", backtrack: {backtrack}")?;
        }
        write!(f, " }}")
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
        write!(f, "{s}")
    }
}

impl<DB: Mirs + ?Sized> DatabasedDisplay<DB> for PlaceOrigin {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            PlaceOrigin::Node(n) => dbwrite!(f, db, "node {}", n),
            PlaceOrigin::Ambient(_, n) => dbwrite!(f, db, "ambient {}", n),
            PlaceOrigin::Expr(n, i) => dbwrite!(f, db, "expr {}:{}", &n.0, &i.as_usize()),
            PlaceOrigin::PolyLen => write!(f, "polylen"),
            PlaceOrigin::Ret => write!(f, "ret"),
            PlaceOrigin::Arg => write!(f, "arg"),
        }
    }
}

impl<DB: Mirs + ?Sized> DatabasedDisplay<DB> for Function {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        for (place_ref, place) in self.iter_places() {
            if let Place::Stack(st) = self.place(place_ref).place {
                dbwrite!(f, db, "// origin: {}\n", &self.stack(st))?;
            }
            if self.place(place_ref).remove_bt {
                writeln!(f, "// remove_bt")?;
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
        }
        Ok(())
    }
}

impl Display for Strictness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Strictness::Return => write!(f, "return"),
            Strictness::Static(c) => write!(f, "static {c}"),
        }
    }
}

impl Display for MirKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MirKind::Call(req) => write!(f, "call {}", req),
            MirKind::Len => write!(f, "len"),
        }
    }
}

pub fn print_all_mir<DB: Mirs, W: Write>(
    db: &DB,
    w: &mut W,
    include_strictness: bool,
) -> std::io::Result<()> {
    let convert_error_ignore = |e| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("could not get mir: {e}"),
        )
    };
    for pd in db.all_parserdefs() {
        dbwrite!(
            w,
            db,
            "---\nmir parserdef {}:\n",
            &db.def_name(pd.0).unwrap()
        )?;
        let fun = db
            .mir(FunKind::ParserDef(pd), MirKind::Call(RequirementSet::all()))
            .map_err(convert_error_ignore)?;
        dbwrite!(w, db, "{}", &fun)?;
        if include_strictness {
            writeln!(w, "--strictness:")?;
            let strictness = db
                .strictness(FunKind::ParserDef(pd), MirKind::Call(RequirementSet::all()))
                .map_err(convert_error_ignore)?;
            for ((place, _), strictness) in fun.iter_places().zip(strictness.iter()) {
                dbwrite!(w, &(&fun, db), "{}: {}\n", &place, &strictness)?;
            }
        }

        dbwrite!(
            w,
            db,
            "---\nmir len parserdef {}:\n",
            &db.def_name(pd.0).unwrap()
        )?;
        let fun = db
            .mir(FunKind::ParserDef(pd), MirKind::Len)
            .map_err(convert_error_ignore)?;
        dbwrite!(w, db, "{}", &fun)?;
        if include_strictness {
            writeln!(w, "--strictness:")?;
            let strictness = db
                .strictness(FunKind::ParserDef(pd), MirKind::Len)
                .map_err(convert_error_ignore)?;
            for ((place, _), strictness) in fun.iter_places().zip(strictness.iter()) {
                dbwrite!(w, &(&fun, db), "{}: {}\n", &place, &strictness)?;
            }
        }

        for block in db.all_parserdef_blocks(pd).iter() {
            let fun = db
                .mir(FunKind::Block(*block), MirKind::Call(RequirementSet::all()))
                .map_err(convert_error_ignore)?;
            dbwrite!(w, db, "---\nmir block {}:\n", &block.0)?;
            dbwrite!(w, db, "{}", &fun)?;
            if include_strictness {
                writeln!(w, "--strictness:")?;
                let strictness = db
                    .strictness(FunKind::Block(*block), MirKind::Call(RequirementSet::all()))
                    .map_err(convert_error_ignore)?;
                for ((place, _), strictness) in fun.iter_places().zip(strictness.iter()) {
                    dbwrite!(w, &(&fun, db), "{}: {}\n", &place, &strictness)?;
                }
            }

            let fun = db
                .mir(FunKind::Block(*block), MirKind::Len)
                .map_err(convert_error_ignore)?;
            dbwrite!(w, db, "---\nmir len block {}:\n", &block.0)?;
            dbwrite!(w, db, "{}", &fun)?;
            if include_strictness {
                writeln!(w, "--strictness:")?;
                let strictness = db
                    .strictness(FunKind::Block(*block), MirKind::Len)
                    .map_err(convert_error_ignore)?;
                for ((place, _), strictness) in fun.iter_places().zip(strictness.iter()) {
                    dbwrite!(w, &(&fun, db), "{}: {}\n", &place, &strictness)?;
                }
            }
        }
    }
    Ok(())
}

fn mir_graph<DB: Mirs, W: Write>(
    db: &DB,
    w: &mut W,
    fun: FunKind,
    prefix: &str,
) -> std::io::Result<()> {
    let mir = db.mir(fun, MirKind::Call(RequirementSet::all())).unwrap();
    writeln!(w, "subgraph cluster_{prefix} {{")?;
    writeln!(w, "\tlabel=\"{prefix}\"")?;
    let idx = |bbref: BBRef| bbref.as_index() + 1;
    for (bbref, bb) in mir.iter_bb() {
        write!(
            w,
            "\t{prefix}_bb{bb} [label=\"{{<start>bb{bb}",
            bb = idx(bbref)
        )?;
        for ins in bb.ins.iter() {
            write!(w, "|")?;
            if ins.is_terminator() {
                write!(w, "<end>")?;
            }
            let ins_str = dbformat!(&(&mir, db), "{}", ins);
            let sanitized = ins_str
                .replace('|', "\\|")
                .replace('{', "\\{")
                .replace('}', "\\}");

            write!(w, "{}", sanitized)?;
        }
        writeln!(w, "}}\"];")?;
    }
    for (bbref, bb) in mir.iter_bb() {
        for ins in bb.ins.iter() {
            let Some(flow) = ins.control_flow() else {
                continue;
            };
            writeln!(
                w,
                "\t{prefix}_bb{}:end -> {prefix}_bb{}:start [color=green];",
                idx(bbref),
                idx(flow.next),
            )?;
            if let Some(bt) = flow.backtrack {
                writeln!(
                    w,
                    "\t{prefix}_bb{}:end -> {prefix}_bb{}:start [color=red];",
                    idx(bbref),
                    idx(bt),
                )?;
            }
        }
    }
    writeln!(w, "}}")
}

pub fn print_all_mir_graphs<DB: Mirs, W: Write>(db: &DB, w: &mut W) -> std::io::Result<()> {
    writeln!(w, "digraph mir {{")?;
    writeln!(w, "node [shape=record];")?;
    for pd in db.all_parserdefs() {
        let pd_name = dbformat!(db, "{}", &pd.0).replace(|c: char| !c.is_ascii_alphanumeric(), "_");
        mir_graph(db, w, FunKind::ParserDef(pd), &pd_name)?;
        for block in db.all_parserdef_blocks(pd).iter() {
            let block_name =
                dbformat!(db, "{}", &block.0).replace(|c: char| !c.is_ascii_alphanumeric(), "_");
            mir_graph(db, w, FunKind::Block(*block), &block_name)?;
        }
    }
    writeln!(w, "}}")
}
