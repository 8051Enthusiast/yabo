use yaboc_base::{dbformat, interner::Identifier};
use yaboc_target::layout::PSize;

use crate::{
    AbsLayoutCtx, ILayout, LayoutSlice, Layouts,
    collect::{EvalType, LCallReq},
    represent::truncated_hex,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum ParserFunKind {
    Wrapper,
    TailWrapper,
    Worker,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum LayoutPart<'comp> {
    Parse(LCallReq, ParserFunKind, ILayout<'comp>),
    Field(Identifier),
    VTable,
    VTableTy,
    Start,
    End,
    Deref,
    SingleForward,
    CurrentElement,
    ArrayLen,
    Skip,
    Span,
    InnerArray,
    CreateArgs(LayoutSlice<'comp>),
    SetArg(PSize),
    Len,
    Mask,
    EvalFun(LCallReq, ParserFunKind),
}

impl<'comp> LayoutPart<'comp> {
    pub fn symbol_str<DB: Layouts + ?Sized>(
        &self,
        ctx: &mut AbsLayoutCtx<'comp>,
        db: &DB,
    ) -> String {
        match self {
            LayoutPart::Parse(reqs, kind, from) => {
                let mut res = String::new();
                res.push_str("parse_");
                let hash = ctx.dcx.layout_hash(db, *from);
                res.push_str(&truncated_hex(&hash));
                let v = match reqs.val {
                    EvalType::NoValue => "_",
                    EvalType::Value => "_v",
                    EvalType::Force => "_f",
                };
                res.push_str(v);
                if reqs.len {
                    res.push('l');
                }
                if reqs.bt {
                    res.push('b');
                }
                match kind {
                    ParserFunKind::Wrapper => {}
                    ParserFunKind::TailWrapper => res.push_str("_tail"),
                    ParserFunKind::Worker => res.push_str("_worker"),
                }
                res
            }
            LayoutPart::Field(n) => dbformat!(db, "field_{}", n),
            LayoutPart::VTable => String::from("vtable"),
            LayoutPart::VTableTy => String::from("vtable_ty"),
            LayoutPart::Start => String::from("start"),
            LayoutPart::End => String::from("end"),
            LayoutPart::Deref => String::from("deref"),
            LayoutPart::SingleForward => String::from("single_forward"),
            LayoutPart::CurrentElement => String::from("current_element"),
            LayoutPart::ArrayLen => String::from("array_len"),
            LayoutPart::Skip => String::from("skip"),
            LayoutPart::Span => String::from("span"),
            LayoutPart::InnerArray => String::from("inner_array"),
            LayoutPart::CreateArgs(p) => {
                format!(
                    "create_args_{}",
                    truncated_hex(&ctx.dcx.layout_slice_hash(db, p))
                )
            }
            LayoutPart::SetArg(idx) => format!("set_arg_{idx}"),
            LayoutPart::Len => String::from("len"),
            LayoutPart::Mask => String::from("mask"),
            LayoutPart::EvalFun(reqs, kind) => {
                let mut res = String::from("eval_fun_");
                let v = match reqs.val {
                    EvalType::NoValue => "",
                    EvalType::Value => "v",
                    EvalType::Force => "f",
                };
                res.push_str(v);
                if reqs.bt {
                    res.push('b');
                }
                match kind {
                    ParserFunKind::Wrapper => {}
                    ParserFunKind::TailWrapper => res.push_str("_tail"),
                    ParserFunKind::Worker => res.push_str("_worker"),
                }
                res
            }
        }
    }
}
