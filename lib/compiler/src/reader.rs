use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Bracket {
    Paren,
    Square,
    Curly,
}

impl Bracket {
    fn open(self) -> char {
        match self {
            Bracket::Paren => '(',
            Bracket::Square => '[',
            Bracket::Curly => '{',
        }
    }

    fn close(self) -> char {
        match self {
            Bracket::Paren => ')',
            Bracket::Square => ']',
            Bracket::Curly => '}',
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum SExp {
    List(Bracket, Vec<SExp>),
    Ident(String),
    Keyword(String),
    Quote(Box<SExp>),
    Number(i64),
}

impl fmt::Display for SExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use SExp::*;

        match self {
            Ident(x) => f.write_str(x),
            Keyword(x) => write!(f, ":{}", x),
            Quote(e) => {
                f.write_str("'")?;
                e.fmt(f)
            }
            Number(x) => write!(f, "{}", x),
            List(b, es) => {
                write!(f, "{}", b.open())?;
                for (i, e) in es.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    e.fmt(f)?;
                }
                write!(f, "{}", b.close())
            }
        }
    }
}

fn is_ident_sym(c: char) -> bool {
    "@!#$%^&*_+-=/<>?".contains(c)
}

peg::parser! {
    grammar sexp_parser() for str {
        use super::SExp::*;
        use Bracket::*;

        rule whitespace() = quiet!{[' ' | '\n' | '\t' | '\r']+}
        rule line_comment() = quiet!{[';'] [^'\n']+}
        rule _() = (whitespace() (line_comment() whitespace())?)?

        pub(crate) rule ident() -> String
            = quiet!{x:$((['a'..='z' | 'A'..='Z' | '0'..='9' ]
                          / [c if is_ident_sym(c)])+) { x.to_string() }}
            / expected!("identifier")

        pub(crate) rule num() -> i64
            = quiet!{x:$(['-']? ['0'..='9']+) {? x.parse().or(Err("i64")) }}
            / expected!("number")

        pub(crate) rule bracket(bracket: Bracket) -> Vec<SExp>
            = [c if c == bracket.open()] _ xs:(sexp()*) [c if c == bracket.close()] { xs }

        pub(crate) rule sexp() -> SExp
            = n:num() _ { Number(n) }
            / x:ident() _ { Ident(x) }
            / ['\''] x:sexp() _ { Quote(Box::new(x)) }
            / [':'] x:ident() _ { Keyword(x) }
            / xs:bracket(Paren) _ { List(Paren, xs) }
            / xs:bracket(Square) _ { List(Square, xs) }
            / xs:bracket(Curly) _ { List(Curly, xs) }

        pub rule read() -> SExp = sexp()
        pub rule read_many() -> Vec<SExp>
            = _ es:sexp()* { es }
    }
}

pub use sexp_parser::read;
pub use sexp_parser::read_many;

#[cfg(test)]
mod tests {
    use crate::reader::sexp_parser::*;
    use crate::reader::*;

    fn n(n: i64) -> SExp {
        SExp::Number(n)
    }
    fn i(x: &'static str) -> SExp {
        SExp::Ident(x.to_string())
    }
    fn k(x: &'static str) -> SExp {
        SExp::Keyword(x.to_string())
    }
    fn q(x: SExp) -> SExp {
        SExp::Quote(Box::new(x))
    }
    fn p(xs: Vec<SExp>) -> SExp {
        SExp::List(Bracket::Paren, xs)
    }
    fn s(xs: Vec<SExp>) -> SExp {
        SExp::List(Bracket::Square, xs)
    }
    fn c(xs: Vec<SExp>) -> SExp {
        SExp::List(Bracket::Curly, xs)
    }

    #[test]
    fn idents() {
        let idents = &["Hello_there!", "cool-1235?", "+", "optional*"];
        for &i in idents {
            assert_eq!(ident(i).unwrap(), i);
        }
    }

    #[test]
    fn ident_vs_num() {
        assert_eq!(sexp("wow").unwrap(), i("wow"));
        assert_eq!(sexp("-wow-!").unwrap(), i("-wow-!"));
        assert_eq!(sexp("-").unwrap(), i("-"));
        assert_eq!(sexp("1003").unwrap(), n(1003));
        assert_eq!(sexp("0").unwrap(), n(0));
        assert_eq!(sexp("-3").unwrap(), n(-3));
    }

    #[test]
    fn test_lists() {
        assert_eq!(sexp("()").unwrap(), p(vec![]));
        assert_eq!(sexp("(a)").unwrap(), p(vec![i("a")]));
        assert_eq!(sexp("(a b)").unwrap(), p(vec![i("a"), i("b")]));
        assert_eq!(sexp("(1 2 3)").unwrap(), p(vec![n(1), n(2), n(3)]));

        assert_eq!(sexp("()").unwrap(), p(vec![]));
        assert_eq!(sexp("(())").unwrap(), p(vec![p(vec![])]));
        assert_eq!(sexp("(() ())").unwrap(), p(vec![p(vec![]), p(vec![])]));
        assert_eq!(
            sexp("([] {} ())").unwrap(),
            p(vec![s(vec![]), c(vec![]), p(vec![])])
        );

        assert_eq!(
            sexp("(if (= 1 2) (print [1 2 3]) (error {1 2}))").unwrap(),
            p(vec![
                i("if"),
                p(vec![i("="), n(1), n(2)]),
                p(vec![i("print"), s(vec![n(1), n(2), n(3)])]),
                p(vec![i("error"), c(vec![n(1), n(2)])])
            ])
        );
    }

    #[test]
    fn quote_and_kws() {
        assert_eq!(sexp("'x").unwrap(), q(i("x")));
        assert_eq!(sexp("'(1 2 3)").unwrap(), q(p(vec![n(1), n(2), n(3)])));
        assert_eq!(sexp(":a").unwrap(), k("a"));
        assert_eq!(sexp("':a").unwrap(), q(k("a")));
    }
}
