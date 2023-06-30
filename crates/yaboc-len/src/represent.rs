use crate::{PolyOp, Term, Val};

impl<T: std::fmt::Debug> std::fmt::Display for Term<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Pd(pd) => write!(f, "parserdef {:?}", pd),
            Term::Arg(idx) => write!(f, "arg {}", idx),
            Term::Apply([fun, arg]) => write!(f, "apply [{}] [{}]", fun, arg),
            Term::Const(c) => write!(f, "const {}", c),
            Term::Opaque => write!(f, "opaque"),
            Term::OpaqueUn(inner) => write!(f, "op [{}]", inner),
            Term::OpaqueBin([lhs, rhs]) => write!(f, "[{}] op [{}]", lhs, rhs),
            Term::Mul([lhs, rhs]) => write!(f, "[{}] * [{}]", lhs, rhs),
            Term::Add([lhs, rhs]) => write!(f, "[{}] + [{}]", lhs, rhs),
            Term::Neg(inner) => write!(f, "-[{}]", inner),
            Term::Arr => write!(f, "arr"),
            Term::Unify([lhs, rhs]) => write!(f, "[{}] = [{}]", lhs, rhs),
            Term::UnifyDyn([lhs, rhs]) => write!(f, "[{}] ~ [{}]", lhs, rhs),
            Term::Copy(inner) => write!(f, "copy [{}]", inner),
        }
    }
}

impl<P: std::fmt::Debug> std::fmt::Display for Val<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Undefined => write!(f, "undef"),
            Val::Const(0, c) => write!(f, "{}", c),
            Val::Const(a, c) => write!(f, "{:0w$} →	{con}", 0, w = *a as usize, con = c),
            Val::Arg(0, idx) => write!(f, "arg {}", idx),
            Val::Arg(a, i) => write!(f, "{:0w$} → arg {i}", 0, w = *a as usize, i = i),
            Val::PolyOp(PolyOp::Add([lhs, rhs])) => write!(f, "[{}] + [{}]", lhs, rhs),
            Val::PolyOp(PolyOp::Mul([lhs, rhs])) => write!(f, "[{}] * [{}]", lhs, rhs),
            Val::PolyOp(PolyOp::Neg(inner)) => write!(f, "-[{}]", inner),
            Val::Poly(0, _, _) => write!(f, "poly"),
            Val::Poly(a, _, b) => {
                for i in 0..*a {
                    write!(f, "{}", b[i as usize] as u8)?;
                }
                write!(f, " → poly")
            }
            Val::PartialPolyApply(0, [lhs, rhs], _, _) => write!(f, "[{}] [{}]", lhs, rhs),
            Val::PartialPolyApply(a, [lhs, rhs], _, b) => {
                for i in 0..*a {
                    write!(f, "{}", b[i as usize] as u8)?;
                }
                write!(f, " → [{}] [{}]", lhs, rhs)
            }
            Val::Static(0, _) => write!(f, "static"),
            Val::Static(a, b) => {
                for i in 0..*a {
                    write!(f, "{}", b[i as usize] as u8)?;
                }
                write!(f, " → static")
            }
            Val::Dynamic => write!(f, "dyn"),
        }
    }
}

pub fn len_graph<P: std::fmt::Debug, T: std::fmt::Debug + Clone>(
    prefix: &str,
    terms: &[Term<P>],
    val: &[Val<T>],
    arities: &[usize],
) -> String {
    let mut ret = String::new();
    for (i, ((term, val), arity)) in terms.iter().zip(val.iter()).zip(arities.iter()).enumerate() {
        ret.push_str(&format!(
            "{prefix}{i} [label=\"{{[{i}]|{arity}}}|<t>{term}|<v>{val}\"];\n"
        ));
    }
    for (i, val) in val.iter().enumerate() {
        for j in val.ref_indices() {
            ret.push_str(&format!("{prefix}{i}:v -> {prefix}{j}:v [color=gray];\n"));
        }
    }
    for (i, term) in terms.iter().enumerate() {
        for j in term.ref_indices() {
            ret.push_str(&format!("{prefix}{i}:t -> {prefix}{j}:t;\n"));
        }
    }
    ret
}
