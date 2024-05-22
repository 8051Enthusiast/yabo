use regex_syntax::hir::{Class, Hir, HirKind, Literal, RepetitionKind, RepetitionRange};

pub type RegexError = regex_syntax::Error;

pub fn regex_len(regex: &Hir) -> Option<i128> {
    match regex.kind() {
        HirKind::Empty => Some(0),
        HirKind::Literal(l) => match l {
            Literal::Unicode(c) => Some(c.len_utf8() as i128),
            Literal::Byte(_) => Some(1),
        },
        HirKind::Class(class) => match class {
            Class::Unicode(r) => {
                let first = r.ranges().first()?.start().len_utf8();
                let last = r.ranges().last()?.end().len_utf8();
                if first == last {
                    Some(first as i128)
                } else {
                    None
                }
            }
            Class::Bytes(_) => Some(1),
        },
        HirKind::Anchor(_) => Some(0),
        HirKind::WordBoundary(_) => Some(0),
        HirKind::Repetition(rep) => match rep.kind {
            RepetitionKind::Range(RepetitionRange::Exactly(c)) => {
                Some(regex_len(&rep.hir)?.checked_mul(c as i128)?)
            }
            RepetitionKind::Range(RepetitionRange::Bounded(start, end)) if start == end => {
                Some(regex_len(&rep.hir)?.checked_mul(start as i128)?)
            }
            _ => None,
        },
        HirKind::Group(g) => regex_len(&g.hir),
        HirKind::Concat(cat) => {
            let mut len = 0i128;
            for hir in cat.iter() {
                len = len.checked_add(regex_len(hir)?)?;
            }
            Some(len)
        }
        HirKind::Alternation(alt) => {
            let mut len = None;
            for hir in alt.iter() {
                let hir_len = regex_len(hir)?;
                if let Some(l) = len {
                    if l != hir_len {
                        return None;
                    }
                } else {
                    len = Some(hir_len);
                }
            }
            len
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{regex_len as regex_len_impl, RegexError};

    fn regex_len(regex: &str) -> Result<Option<i128>, Box<RegexError>> {
        let regex = regex_syntax::ParserBuilder::new()
            .allow_invalid_utf8(true)
            .dot_matches_new_line(true)
            .case_insensitive(false)
            .build()
            .parse(regex)?;
        Ok(regex_len_impl(&regex))
    }

    #[test]
    fn simple_string() {
        assert_eq!(regex_len("abc").unwrap(), Some(3));
    }

    #[test]
    fn simple_string_with_unicode() {
        assert_eq!(regex_len("aβc").unwrap(), Some(4));
    }

    #[test]
    fn unicode_class() {
        assert_eq!(regex_len("[aβ]").unwrap(), None);
        assert_eq!(regex_len("[bαa]").unwrap(), None);
    }

    #[test]
    fn alternation() {
        assert_eq!(regex_len("a|b").unwrap(), Some(1));
        assert_eq!(regex_len("(a|b)c").unwrap(), Some(2));
        assert_eq!(regex_len("a|b|c").unwrap(), Some(1));
        assert_eq!(regex_len("a|b|cd").unwrap(), None);
    }

    #[test]
    fn repetition() {
        assert_eq!(regex_len("a*").unwrap(), None);
        assert_eq!(regex_len("(xyz){2}").unwrap(), Some(6));
        assert_eq!(regex_len("(xyz){2,2}").unwrap(), Some(6));
        assert_eq!(regex_len("(xyz){2,3}").unwrap(), None);
    }
}
