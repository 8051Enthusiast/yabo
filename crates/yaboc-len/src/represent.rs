use std::fmt::Write;

use crate::{
    depvec::{self, DepVec, IndexDepVec},
    PolyOp, SizeExpr, Term, Val,
};

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
            Term::Cat([lhs, rhs]) => write!(f, "[{}] ++ [{}]", lhs, rhs),
            Term::Neg(inner) => write!(f, "-[{}]", inner),
            Term::Arr => write!(f, "arr"),
            Term::Unify([lhs, rhs]) => write!(f, "[{}] = [{}]", lhs, rhs),
            Term::UnifyDyn([lhs, rhs]) => write!(f, "[{}] ~ [{}]", lhs, rhs),
            Term::Then([lhs, rhs]) => write!(f, "[{}] then [{}]", lhs, rhs),
            Term::Copy(inner) => write!(f, "copy [{}]", inner),
            Term::Size(true, inner) => write!(f, "exact-size [{}]", inner),
            Term::Size(false, inner) => write!(f, "size [{}]", inner),
            Term::ScopeIntro(_) => write!(f, "scope-intro"),
            Term::BlockEnd(_, len_loc) => write!(f, "block [{}]", len_loc),
            Term::FunctionEnd(_, len_loc) => write!(f, "function [{}]", len_loc),
            Term::Parsed => write!(f, "parsed"),
            Term::Span => write!(f, "span"),
            Term::Backtracking(inner) => write!(f, "backtrack [{}]", inner),
        }
    }
}

fn write_deps<const REVERSED: bool>(
    f: &mut impl Write,
    deps: &DepVec<REVERSED>,
    rank: u32,
) -> std::fmt::Result {
    for i in 0..rank {
        if deps.has_len(i as usize) {
            write!(f, "¹")?;
        } else {
            write!(f, "⁰")?;
        }
        //write!(f, "\u{0338}")?;
        if deps.has_val(i as usize) {
            write!(f, "₁")?;
        } else {
            write!(f, "₀")?;
        }
    }
    Ok(())
}

impl<P: std::fmt::Debug> std::fmt::Display for Val<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Undefined => write!(f, "undef"),
            Val::Const(0, c, _) => write!(f, "{}", c),
            Val::Const(a, c, deps) => {
                write_deps(f, deps, *a)?;
                write!(f, " → {c}")
            }
            Val::Arg(0, idx) => write!(f, "arg {}", idx),
            Val::Arg(a, i) => {
                write_deps(f, &IndexDepVec::default(), *a)?;
                write!(f, " → arg {i}")
            }
            Val::PolyOp(PolyOp::Add([lhs, rhs])) => write!(f, "[{}] + [{}]", lhs, rhs),
            Val::PolyOp(PolyOp::Mul([lhs, rhs])) => write!(f, "[{}] * [{}]", lhs, rhs),
            Val::PolyOp(PolyOp::Neg(inner)) => write!(f, "-[{}]", inner),
            Val::Poly(0, _, _) => write!(f, "poly"),
            Val::Poly(a, _, b) => {
                write_deps(f, b, *a)?;
                write!(f, " → poly")
            }
            Val::PartialPolyApply(0, [lhs, rhs], _, _) => write!(f, "[{}] [{}]", lhs, rhs),
            Val::PartialPolyApply(a, [lhs, rhs], _, b) => {
                write_deps(f, b, *a)?;
                write!(f, " → [{}] [{}]", lhs, rhs)
            }
            Val::Static(0, _) => write!(f, "static"),
            Val::Static(a, b) => {
                write_deps(f, b, *a)?;
                write!(f, " → static")?;
                Ok(())
            }
            Val::Unsized => write!(f, "unsized"),
        }
    }
}

pub fn len_graph<P: std::fmt::Debug, T: std::fmt::Debug + Clone>(
    prefix: &str,
    terms: &SizeExpr<P>,
    val: &[Val<T>],
    root: usize,
    arg_deps: &[depvec::ArgDeps],
) -> String {
    let mut ret = String::new();
    let reqs = terms.reqs(root, val);
    for i in 0..terms.terms.len() {
        let arg_depth = terms.arg_depth[i] as usize;
        let mut dep_strings = [String::new(), String::new(), String::new()];
        let arg_dep = &arg_deps[i];
        for (dep_string, deps) in dep_strings.iter_mut().zip(arg_dep.0.iter()) {
            write_deps(dep_string, deps, arg_depth as u32).unwrap();
        }
        let [lendep, valdep, btdep] = dep_strings;
        let term = &terms.terms[i];
        let req = &reqs[i];
        let val = &val[i];
        ret.push_str(&format!(
            "{prefix}{i} [label=\"{{[{i}]|{req}}}|<t>{term}|<v>{val}|{{l {lendep}|v {valdep}|b {btdep}}}\"];\n"
        ));
    }
    for (i, val) in val.iter().enumerate() {
        for j in val.ref_indices() {
            ret.push_str(&format!("{prefix}{i}:v -> {prefix}{j}:v [color=gray];\n"));
        }
    }
    for (i, term) in terms.terms.iter().enumerate() {
        for j in term.ref_indices() {
            ret.push_str(&format!("{prefix}{i}:t -> {prefix}{j}:t;\n"));
        }
    }
    ret
}
