
#[derive(Debug)]
enum SExp {
    List(Vec<SExp>),
    Ident(String),
    Number(i64),
}

fn is_ident_sym(c: char) -> bool {
    "!#$%^&*_+-=/<>?".contains(c)
}

peg::parser!{
    grammar sexp_parser() for str {
        use super::*;

        rule whitespace() = quiet!{[' ' | '\n' | '\t' | '\r']+}
        rule line_comment() = quiet!{[';'] [^'\n']+}
        rule _() = whitespace() (line_comment() whitespace())?

        pub(crate) rule ident() -> String
            = quiet!{x:$((['a'..='z' | 'A'..='Z' | '0'..='9' ] / [c if is_ident_sym(c)])+) { x.to_string() }}
            / expected!("identifier")

        pub rule sexp() -> SExp
            = x:ident() _ { SExp::Ident(x) }

        pub rule programs() -> Vec<SExp>
            = _ es:sexp()* { es }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn idents() {
        let idents = &[
            "Hello_there!",
            "cool-1235?",
            "+",
            "optional*",
        ];
        for &i in idents  {
            assert_eq!(sexp_parser::ident(i).unwrap(), i);
        }
    }
}
