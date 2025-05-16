use crate::reader::SExp::*;
use crate::reader::{Bracket, SExp};

#[derive(Debug, PartialEq, Eq)]
pub struct Binding {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Var(String),
    Invoke(Box<Expr>, Vec<Expr>),
    Lit(Literal),
    Let(Vec<Binding>, Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Keyword(String),
    Symbol(String),
    Num(i64),
    Nil,
    True,
    False,
}

use Expr::*;
use Literal::*;

type ParseError = String;
type Result<T> = std::result::Result<T, ParseError>;

const SPECIAL_FORMS: &'static [&'static str] =
    &["fn", "if", "let", "def", "do", "quote", "loop", "recur"];

fn is_special_form_ident(x: &str) -> bool {
    SPECIAL_FORMS.contains(&x)
}

impl Expr {
    pub fn parse(e: &SExp) -> Result<Expr> {
        Ok(match e {
            Ident(x) if is_special_form_ident(x) => todo!("special form ident"),
            Ident(x) => match &**x {
                "nil" => Lit(Nil),
                "true" => Lit(True),
                "false" => Lit(False),
                _ => Var(x.clone()),
            },
            Number(n) => Lit(Num(*n)),
            List(Bracket::Paren, es) => {
                if es.len() == 0 {
                    todo!("empty list")
                }

                if let Ident(head) = &es[0] {
                    match &**head {
                        "let" => return Expr::parse_let(es),
                        "if" => return Expr::parse_if(es),
                        _ => {}
                    }
                }

                Invoke(
                    Box::new(Expr::parse(&es[0])?),
                    es[1..]
                        .iter()
                        .map(Expr::parse)
                        .collect::<Result<Vec<_>>>()?,
                )
            }
            List(_, es) => todo!("non-paren lists"),
            SExp::Keyword(kw) => Lit(Literal::Keyword(kw.clone())),
            Quote(e) => todo!("quotes"),
        })
    }

    fn parse_let(es: &[SExp]) -> Result<Expr> {
        if es.len() < 2 {
            return Err("Expected bindings for `let`".to_string());
        }

        match &es[1] {
            List(Bracket::Square, bs) => {
                let (pairs, remaining) = bs.as_chunks::<2>();
                if remaining.len() != 0 {
                    return Err("`let` has a binding missing a value".to_string());
                }
                let mut bindings = Vec::new();
                for [x, v] in pairs {
                    let Ident(x) = x else {
                        return Err("Expected a symbol for `let` binding left-hand side".to_string());
                    };
                    let e = Expr::parse(v)?;
                    bindings.push(Binding {
                        name: x.to_string(),
                        expr: e,
                    });
                }
                let body = es[2..]
                    .into_iter()
                    .map(Expr::parse)
                    .collect::<Result<Vec<_>>>()?;
                Ok(Expr::Let(bindings, body))
            }
            _ => return Err("Expected `let` bindings, use `[]` for the bindings".to_string()),
        }
    }

    fn parse_if(es: &[SExp]) -> Result<Expr> {
        if es.len() != 4  {
            return Err("Expected exactly three values for `if`, a conditon, then-branch and else-branch".to_string());
        }

        Ok(Expr::If(
            Box::new(Expr::parse(&es[1])?),
            Box::new(Expr::parse(&es[2])?),
            Box::new(Expr::parse(&es[3])?),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::reader::{read, read_many};

    use super::*;

    fn p(src: &str) -> Result<Expr> {
        Expr::parse(&read(src).unwrap())
    }

    #[test]
    fn test_lit() {
        assert_eq!(p("3"), Ok(Lit(Num(3))));
        assert_eq!(p("-0"), Ok(Lit(Num(0))));
        assert_eq!(p("true"), Ok(Lit(True)));
        assert_eq!(p("false"), Ok(Lit(False)));
        assert_eq!(p("nil"), Ok(Lit(Nil)));
        assert_eq!(p(":woo"), Ok(Lit(Literal::Keyword("woo".to_string()))));
    }

    #[test]
    fn test_invoke() {
        assert_eq!(
            p("(+ 1 2 3)"),
            Ok(Invoke(
                Box::new(Var("+".to_string())),
                vec![Lit(Num(1)), Lit(Num(2)), Lit(Num(3))]
            ))
        );
        assert_eq!(
            p("(+ (+ 1 2) 3)"),
            Ok(Invoke(
                Box::new(Var("+".to_string())),
                vec![
                    Invoke(
                        Box::new(Var("+".to_string())),
                        vec![Lit(Num(1)), Lit(Num(2)),]
                    ),
                    Lit(Num(3))
                ]
            ))
        );
    }
}
