use vm::ext::Externals;

use crate::ast::*;
use crate::lowering::Hir;
use crate::scope::Scope;

struct VarEntry {
    name: String,
}

struct BuiltinEntry {
    name: String,
    extcall_id: u16,
}

enum Entry {
    Var(VarEntry),
    Builtin(BuiltinEntry),
}

struct Compiler {
    scope: Scope<Entry>,
}

type CompilerError = String;
type Result<T> = std::result::Result<T, CompilerError>;

impl Compiler {
    fn new() -> Compiler {
        let mut scope = Scope::new();

        scope.enter();
        let builtins = &[("i32add", Externals::I32_ADD)];
        for (n, i) in builtins {
            scope.define(n, Entry::Builtin(BuiltinEntry {
                name: n.to_string(),
                extcall_id: *i,
            }));
        }

        Compiler {
            scope,
        }
    }

    pub fn compile(e: Expr) -> Result<Hir> {
        let mut c = Compiler::new();
        c.expr(e)
    }

    fn expr(&mut self, e: Expr) -> Result<Hir> {
        match e {
            Expr::Var(x) => todo!("vars"),
            Expr::Invoke(exprs) => todo!("invokes"),
            Expr::Lit(l) => self.lit(l),
        }
    }

    fn lit(&mut self, lit: Literal) -> Result<Hir> {
        match lit {
            Literal::Num(n) if n >= i32::MIN as i64 && n <= i32::MAX as i64 => {
                // TODO: We can technically fit 45bit integers
                Ok(Hir::I32(n as i32))
            }
            Literal::Num(n) => todo!("large integers"),
            Literal::Keyword(_) => todo!("keywords"),
            Literal::Symbol(_) => todo!("symbols"),
            Literal::Nil => todo!("nil"),
            Literal::True => todo!("true"),
            Literal::False => todo!("false"),
        }
    }
}
