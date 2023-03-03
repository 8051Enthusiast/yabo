use std::{ops::Range, str::CharIndices};

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Univariate {
    X,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Bivariate {
    X,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinOp {
    Add,
    Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum UnOp {
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Nothing {}

impl ExprKind for Univariate {
    type NiladicOp = PartialEval<Univariate, i64>;
    type MonadicOp = UnOp;
    type DyadicOp = BinOp;
    type VariadicOp = Nothing;
}

impl ExprKind for Bivariate {
    type NiladicOp = PartialEval<Bivariate, i64>;
    type MonadicOp = UnOp;
    type DyadicOp = BinOp;
    type VariadicOp = Nothing;
}

fn skip_whitespace(chars: &mut CharIndices) {
    while chars.as_str().starts_with(char::is_whitespace) {
        chars.next();
    }
}

fn num<'id>(
    chars: &mut CharIndices,
    builder: &mut ExprDataBuilder<'id, Bivariate, Range<usize>>,
) -> Option<ExprRef<'id, Bivariate>> {
    let mut num = 0;
    let mut digits = 0;
    let mut start = None;
    for (i, c) in &mut *chars {
        if c.is_ascii_digit() {
            start = start.or(Some(i));
            digits += 1;
            num = num * 10 + c.to_digit(10).unwrap() as i64;
        } else {
            break;
        }
    }
    start.map(|start| {
        builder.add_expr(
            ExprHead::new_niladic(PartialEval::Eval(num)),
            start..start + digits,
        )
    })
}

fn atom<'id>(
    chars: &mut CharIndices,
    builder: &mut ExprDataBuilder<'id, Bivariate, Range<usize>>,
) -> Option<ExprRef<'id, Bivariate>> {
    skip_whitespace(chars);
    if let Some(span) = recognize(chars, 'x') {
        Some(builder.add_expr(
            ExprHead::new_niladic(PartialEval::Uneval(Bivariate::X)),
            span,
        ))
    } else if let Some(span) = recognize(chars, 'y') {
        Some(builder.add_expr(
            ExprHead::new_niladic(PartialEval::Uneval(Bivariate::Y)),
            span,
        ))
    } else {
        num(chars, builder)
    }
}

fn recognize(chars: &mut CharIndices, c: char) -> Option<Range<usize>> {
    let old_chars = chars.clone();
    skip_whitespace(chars);
    if let Some((i, next)) = chars.next() {
        if next == c {
            Some(i..i + 1)
        } else {
            *chars = old_chars;
            None
        }
    } else {
        None
    }
}

fn sign<'id>(
    chars: &mut CharIndices,
    builder: &mut ExprDataBuilder<'id, Bivariate, Range<usize>>,
) -> Option<ExprRef<'id, Bivariate>> {
    Some(if let Some(span) = recognize(chars, '-') {
        let expr = atom(chars, builder)?;
        builder.add_expr(ExprHead::new_monadic(UnOp::Neg, expr), span)
    } else {
        atom(chars, builder)?
    })
}

fn mul<'id>(
    chars: &mut CharIndices,
    builder: &mut ExprDataBuilder<'id, Bivariate, Range<usize>>,
) -> Option<ExprRef<'id, Bivariate>> {
    let mut lhs = sign(chars, builder)?;
    while let Some(span) = recognize(chars, '*') {
        let rhs = sign(chars, builder)?;
        lhs = builder.add_expr(ExprHead::new_dyadic(BinOp::Mul, [lhs, rhs]), span);
    }
    Some(lhs)
}

fn add<'id>(
    chars: &mut CharIndices,
    builder: &mut ExprDataBuilder<'id, Bivariate, Range<usize>>,
) -> Option<ExprRef<'id, Bivariate>> {
    let mut lhs = mul(chars, builder)?;
    while let Some(span) = recognize(chars, '+') {
        let rhs = mul(chars, builder)?;
        lhs = builder.add_expr(ExprHead::new_dyadic(BinOp::Add, [lhs, rhs]), span);
    }
    Some(lhs)
}

fn parse(s: &str) -> DataExpr<Bivariate, Range<usize>> {
    IdxExpression::build_new_with_data(|builder| {
        let mut chars = s.char_indices();
        skip_whitespace(&mut chars);
        add(&mut chars, builder).is_some()
    })
}

fn univariate_string(s: PartialEval<Univariate, i64>) -> String {
    match s {
        PartialEval::Eval(n) => n.to_string(),
        PartialEval::Uneval(Univariate::X) => "x".to_string(),
    }
}

fn eval(expr: DataRefExpr<Bivariate, Range<usize>>, x: i64, y: i64) -> Result<i64, Range<usize>> {
    let expr = expr.try_partial_eval::<i64, Univariate, _>(
        |n, _| Ok(PartialEval::Eval(n)),
        |_, (head, span)| {
            let span = span.clone();
            Ok(PartialEval::Eval(match head {
                ExprHead::Niladic(PartialEval::Eval(n)) => n,
                ExprHead::Niladic(PartialEval::Uneval(Bivariate::X)) => x,
                ExprHead::Niladic(PartialEval::Uneval(Bivariate::Y)) => y,
                ExprHead::Monadic(op, inner) => {
                    let PartialEval::Eval((n, _)) = inner else {
                            return Ok(PartialEval::Uneval(ExprHead::new_monadic(op, inner)))
                        };
                    n.checked_neg().ok_or(span)?
                }
                ExprHead::Dyadic(op, inner) => {
                    let [PartialEval::Eval((lhs, _)), PartialEval::Eval((rhs, _))] = inner else {
                            return Ok(PartialEval::Uneval(ExprHead::new_dyadic(op, inner)))
                        };
                    match op {
                        BinOp::Add => lhs.checked_add(rhs),
                        BinOp::Mul => lhs.checked_mul(rhs),
                    }
                    .ok_or(span)?
                }
                ExprHead::Variadic(v, _) => match v {},
            }))
        },
    )?;
    let Some(ExprHead::Niladic(PartialEval::Eval(n))) = expr.expr().heads.last() else {
        panic!("expression did not evaluate to a number")
    };
    Ok(*n)
}

fn eval_y(expr: &IdxExpression<Bivariate>, y: i64) -> IdxExpression<Univariate> {
    expr.asref().partial_eval::<_, Univariate>(
        |n, _| PartialEval::Eval(n),
        |_, head| {
            PartialEval::Eval(match head {
                ExprHead::Niladic(PartialEval::Eval(n)) => n,
                ExprHead::Niladic(PartialEval::Uneval(Bivariate::X)) => {
                    return PartialEval::Uneval(ExprHead::new_niladic(
                        PartialEval::Uneval(Univariate::X),
                    ))
                }
                ExprHead::Niladic(PartialEval::Uneval(Bivariate::Y)) => y,
                ExprHead::Monadic(UnOp::Neg, inner) => {
                    let PartialEval::Eval((n, _)) = inner else {
                        return PartialEval::Uneval(ExprHead::new_monadic(UnOp::Neg, inner))
                    };
                    -n
                }
                ExprHead::Dyadic(op, inner) => {
                    let [PartialEval::Eval((lhs, _)), PartialEval::Eval((rhs, _))] = inner else {
                        return PartialEval::Uneval(ExprHead::new_dyadic(op, inner))
                    };
                    match op {
                        BinOp::Add => lhs + rhs,
                        BinOp::Mul => lhs * rhs,
                    }
                }
                ExprHead::Variadic(v, _) => match v {}
            })
        },
    ).into_expr()
}

fn as_string<K>(expr: &IdxExpression<K>, mut f: impl FnMut(K::NiladicOp) -> String) -> String
where
    K: ExprKind<MonadicOp = UnOp, DyadicOp = BinOp, VariadicOp = Nothing>,
{
    expr.asref().fold(|head| match head {
        ExprHead::Niladic(n) => f(n),
        ExprHead::Monadic(UnOp::Neg, inner) => {
            format!("-{}", inner)
        }
        ExprHead::Dyadic(op, inner) => {
            let op = match op {
                BinOp::Add => " + ",
                BinOp::Mul => " * ",
            };
            format!("{}{}{}", inner[0], op, inner[1])
        }
        ExprHead::Variadic(v, _) => match v {},
    })
}

#[test]
fn simple_poly() {
    let expr = parse("3 * x + 2 * -y");
    assert_eq!(eval(expr.take_ref(), 1, 2), Ok(-1));
    assert_eq!(eval(expr.take_ref(), 2, 3), Ok(0));
}

#[test]
fn overflowing_poly() {
    let expr = parse("x * x * x * y");
    assert_eq!(eval(expr.take_ref(), 100000, 50000), Err(10..11))
}

#[test]
fn partial_eval() {
    let expr = parse("4 * x * x + 3 * y * x + 2 * y * y");
    let at_2 = eval_y(&expr.expr, 2);
    assert_eq!(as_string(&at_2, univariate_string), "4 * x * x + 6 * x + 8");
    let at_5 = eval_y(&expr.expr, 5);
    assert_eq!(
        as_string(&at_5, univariate_string),
        "4 * x * x + 15 * x + 50"
    );
    let at_minus_3 = eval_y(&expr.expr, -3);
    assert_eq!(
        as_string(&at_minus_3, univariate_string),
        "4 * x * x + -9 * x + 18"
    );
}
