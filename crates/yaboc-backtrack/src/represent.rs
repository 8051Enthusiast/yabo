use std::fmt::Display;

use yaboc_base::{databased_display::DatabasedDisplay, dbwrite};
use yaboc_types::TypeInterner;

use crate::{EvalEffectKind, ExprNode, Instruction};

impl Display for EvalEffectKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalEffectKind::None => Ok(()),
            EvalEffectKind::Effectful => write!(f, "?"),
            EvalEffectKind::Silent => write!(f, "!"),
        }
    }
}

impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for Instruction {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        match self {
            Instruction::True => write!(f, "true"),
            Instruction::False => write!(f, "false"),
            Instruction::Identity => write!(f, "identity"),
            Instruction::Array => write!(f, "array"),
            Instruction::Single => write!(f, "single"),
            Instruction::None => write!(f, "none"),
            Instruction::Fail => write!(f, "fail"),
            Instruction::Apply(fun, args) => {
                write!(f, "apply [{}](", fun)?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "[{}]", arg)?;
                }
                write!(f, ")")
            }
            Instruction::Eval(effectful, inner) => {
                write!(f, "eval{effectful} [{inner}]")
            }
            Instruction::Parse {
                effectful,
                fun,
                arg,
            } => {
                write!(f, "parse{effectful} [{fun}]([{arg}])")
            }
            Instruction::Copy(inner) => {
                write!(f, "copy [{}]", inner)
            }
            Instruction::Unify(args) => {
                write!(f, "unify [")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, "]")
            }
            Instruction::EnterScope => write!(f, "enter_scope"),
            Instruction::LeaveScope(ret) => write!(f, "leave_scope [{}]", ret),
            Instruction::Activate(fun) => write!(f, "activate [{}]", fun),
            Instruction::Deactivate(fun) => write!(f, "deactivate [{}]", fun),
            Instruction::Arg(arg) => write!(f, "arg {{{}}}", arg),
            Instruction::ParserDef(pd) => dbwrite!(f, db, "parser_def {}", pd),
            Instruction::GetField(inner, field) => {
                write!(f, "get_field [{}]", inner)?;
                dbwrite!(f, db, " . {}", field)
            }
        }
    }
}

impl<DB: TypeInterner + ?Sized> DatabasedDisplay<DB> for ExprNode {
    fn db_fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &DB) -> std::fmt::Result {
        let ExprNode {
            row_range,
            ty,
            bound,
            instr,
        } = self;
        dbwrite!(
            f,
            db,
            "{}\n  row_range: {row_range:?} x {bound}\n  ty: {}",
            instr,
            ty,
        )
    }
}
