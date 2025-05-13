use crate::reader::SExp::*;
use crate::reader::{Bracket, SExp};

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Var(String),
    Invoke(Box<Expr>, Vec<Expr>),
    Lit(Literal),
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
