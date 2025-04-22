use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    num::NonZeroUsize,
};

use crate::{
    ext::{ExtVal, Externals},
    program::*,
    repr::{CombLabel, OperatorLabel, Tag},
    Port,
};

type UInst = UnlinkedInst;

#[derive(Debug)]
pub enum Expr {
    I32(i32),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Lam(Vec<String>, Box<Expr>),
    Tup(Vec<Expr>),
    Untup(Vec<String>, Box<Expr>, Box<Expr>),
    ExtCall(u16, Box<Expr>, Box<Expr>),
    Lift(Box<Expr>),
    Lower(Box<Expr>),
}

pub fn i(i: i32) -> Expr {
    Expr::I32(i)
}
pub fn v(x: &'static str) -> Expr {
    Expr::Var(x.to_string())
}
pub fn letv(x: &'static str, v: Expr, b: Expr) -> Expr {
    Expr::Let(x.to_string(), Box::new(v), Box::new(b))
}
pub fn extcall(lbl: u16, a: Expr, b: Expr) -> Expr {
    Expr::ExtCall(lbl, Box::new(a), Box::new(b))
}
pub fn print(a: Expr, b: Expr) -> Expr {
    Expr::ExtCall(Externals::PRINT, Box::new(a), Box::new(b))
}
pub fn lam(xs: Vec<&'static str>, b: Expr) -> Expr {
    Expr::Lam(xs.iter().map(|x| x.to_string()).collect(), Box::new(b))
}
pub fn call(f: Expr, xs: Vec<Expr>) -> Expr {
    Expr::Call(Box::new(f), xs)
}
pub fn if_(c: Expr, t: Expr, f: Expr) -> Expr {
    Expr::If(Box::new(c), Box::new(t), Box::new(f))
}
pub fn tup(es: Vec<Expr>) -> Expr {
    Expr::Tup(es)
}
pub fn untup(xs: Vec<&'static str>, tup: Expr, body: Expr) -> Expr {
    Expr::Untup(
        xs.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
        Box::new(tup),
        Box::new(body),
    )
}

pub fn def(x: &'static str, e: Expr) -> Def {
    Def {
        name: x.to_string(),
        body: e,
    }
}

#[derive(Debug)]
pub struct Def {
    name: String,
    body: Expr,
}

struct Slot {
    prev: Option<Box<Slot>>,
    name: String,
    users: Vec<Reg>,
}

impl Slot {
    fn new(name: String) -> Slot {
        Slot {
            prev: None,
            name,
            users: Vec::new(),
        }
    }
}

struct Scope {
    layers: Vec<HashMap<String, Slot>>,
}

impl Scope {
    fn new() -> Scope {
        Scope { layers: vec![] }
    }

    fn enter(&mut self) {
        self.layers.push(HashMap::new())
    }

    fn leave(&mut self) {
        self.layers.pop().unwrap();
    }

    fn define(&mut self, n: &str) {
        let slot = Slot::new(n.to_string());
        match self
            .layers
            .last_mut()
            .expect("zero scope layers")
            .entry(n.to_string())
        {
            Entry::Occupied(mut e) => {
                let old_slot = e.insert(slot);
                let _ = e.get_mut().prev.insert(Box::new(old_slot));
            }
            Entry::Vacant(e) => {
                e.insert(slot);
            }
        }
    }

    fn undefine(&mut self, n: &str) -> Vec<Reg> {
        if let Entry::Occupied(mut e) = self
            .layers
            .last_mut()
            .expect("zero scope layers")
            .entry(n.to_string())
        {
            if let Some(prev_slot) = e.get_mut().prev.take() {
                e.insert(*prev_slot).users
            } else {
                e.remove().users
            }
        } else {
            panic!("can't undefine non-exisitng variable `{}`", n);
        }
    }

    fn lookup(&mut self, n: &str) -> Option<&mut Slot> {
        for layer in self.layers.iter_mut().rev() {
            if let Some(slot) = layer.get_mut(n) {
                return Some(slot);
            }
        }

        None
    }
}

pub struct Compiler {
    next_reg: NonZeroUsize,
    dup_count: u16,
    globals: HashSet<String>,
    globals_refs: Vec<String>,
    insts: Vec<UnlinkedInst>,
    scope: Scope,
}

type CompilerError = String;
type Result<T> = std::result::Result<T, CompilerError>;

impl Compiler {
    fn new() -> Compiler {
        Compiler {
            next_reg: NonZeroUsize::new(1).unwrap(),
            globals: HashSet::new(),
            dup_count: 0,
            globals_refs: Vec::new(),
            insts: Vec::new(),
            scope: Scope::new(),
        }
    }

    fn gen(&mut self) -> Reg {
        let r = Reg::new(self.next_reg);
        self.next_reg = self.next_reg.checked_add(1).expect("Register overflow");
        r
    }

    fn val(&mut self, r: Reg, x: i32) {
        self.insts
            .push(UInst::Nilary(r, Port::from_extval(ExtVal::i32(x))));
    }

    fn erase(&mut self, r: Reg) {
        self.insts.push(UInst::Nilary(r, Port::eraser()));
    }

    fn dup(&mut self, x: Reg, x0: Reg, x1: Reg) {
        self.insts.push(UInst::Binary(
            Tag::Comb,
            CombLabel::Dup(self.dup_count).into(),
            x,
            x0,
            x1,
        ));
        self.dup_count = self
            .dup_count
            .checked_add(1)
            .expect("Too many duplicate nodes!");
    }

    fn global(&mut self, r: Reg, g: &str) {
        if let Some(g) = self.globals_refs.iter().position(|x| x == g) {
            self.insts.push(UInst::Global(r, g));
        } else {
            self.insts.push(UInst::Global(r, self.globals_refs.len()));
            self.globals_refs.push(g.to_string());
        }
    }

    fn extcall(&mut self, lbl: u16, x: Reg, y: Reg, r: Reg) {
        self.insts.push(UInst::Binary(Tag::ExtFn, lbl, x, y, r))
    }

    fn app(&mut self, f: Reg, x: Reg, res: Reg) {
        self.insts
            .push(UInst::Binary(Tag::Comb, CombLabel::Fn.into(), f, x, res));
    }

    fn lam(&mut self, x: Reg, b: Reg, res: Reg) {
        self.insts
            .push(UInst::Binary(Tag::Comb, CombLabel::Fn.into(), res, x, b));
    }

    fn branch(&mut self, c: Reg, t: Reg, f: Reg, res: Reg) {
        // c = ?(?(t f) res)
        let tf = self.gen();
        self.insts.push(UInst::Binary(
            Tag::Operator,
            OperatorLabel::Branch.into(),
            c,
            tf,
            res,
        ));
        self.insts.push(UInst::Binary(
            Tag::Operator,
            OperatorLabel::Branch.into(),
            tf,
            t,
            f,
        ));
    }

    fn tup(&mut self, a: Reg, b: Reg, res: Reg) {
        self.insts
            .push(UInst::Binary(Tag::Comb, CombLabel::Tup.into(), res, a, b));
    }

    pub fn compile(defs: Vec<Def>) -> Result<Vec<UnlinkedProgram>> {
        let mut c = Compiler::new();

        // Declare all the globals:
        c.globals.extend(defs.iter().map(|d| d.name.clone()));

        defs.into_iter().map(|d| c.def(d)).collect()
    }

    fn def(&mut self, def: Def) -> Result<UnlinkedProgram> {
        self.scope.enter();
        self.expr(def.body, Reg::ROOT)?;
        let prog = UnlinkedProgram {
            name: def.name,
            globals: self.globals_refs.drain(..).collect(),
            reg_count: self.next_reg.get(),
            instructions: std::mem::replace(&mut self.insts, Vec::new()),
        };
        self.next_reg = NonZeroUsize::new(1).unwrap();
        self.scope.leave();
        debug_assert!(self.scope.layers.is_empty());
        Ok(prog)
    }

    fn gen_expr(&mut self, e: Expr) -> Result<Reg> {
        let r = self.gen();
        self.expr(e, r)?;
        Ok(r)
    }

    fn materialize_users(&mut self, mut users: Vec<Reg>) -> Result<Reg> {
        if let Some(last) = users.pop() {
            if users.is_empty() {
                // If there's exactly one user, just wire it up directly:
                Ok(last)
            } else {
                // Produce:
                //   x = dup(u0 dup(u1 dup(u2 u3)))
                Ok(users.into_iter().rfold(last, |rhs, u| {
                    let r = self.gen();
                    self.dup(r, u, rhs);
                    r
                }))
            }
        } else {
            // If there are no users of a value, we need to erase it
            let r = self.gen();
            self.erase(r);
            Ok(r)
        }
    }

    fn scoped<'a, T>(
        &'a mut self,
        vars: &'a [String],
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<(T, impl Iterator<Item = Vec<Reg>> + 'a)> {
        for x in vars.iter() {
            self.scope.define(x);
        }
        let res = f(self)?;
        let us = vars.into_iter().map(|x| self.scope.undefine(&x));
        Ok((res, us))
    }

    fn expr(&mut self, e: Expr, r: Reg) -> Result<()> {
        use Expr::*;
        println!(">> {:?} <- {:?}", r, e);

        match e {
            I32(x) => self.val(r, x),
            Var(n) => {
                if let Some(slot) = self.scope.lookup(&n) {
                    slot.users.push(r);
                } else {
                    if self.globals.contains(&n) {
                        self.global(r, &n);
                    } else {
                        return Err(format!("Undefined variable `{}`", n));
                    }
                }
            }
            Let(x, v, b) => {
                let user;
                {
                    let vars = &[x];
                    let (_, mut users) = self.scoped(vars, |c| c.expr(*b, r))?;
                    user = users.next().unwrap();
                }
                let var_reg = self.materialize_users(user)?;
                self.expr(*v, var_reg)?;
            }
            ExtCall(extfn, e0, e1) => {
                let a = self.gen_expr(*e0)?;
                let b = self.gen_expr(*e1)?;
                self.extcall(extfn, a, b, r);
            }
            Add(e0, e1) => {
                let a = self.gen_expr(*e0)?;
                let b = self.gen_expr(*e1)?;
                self.extcall(Externals::ADD, a, b, r);
            }
            Call(f, mut es) => {
                if es.is_empty() {
                    // Call `f` with a dummy value
                    es.push(Expr::I32(0));
                }
                // Generate:
                //   f = fn(e0 fn(e1 fn(e2 r)))
                let f_reg = es.into_iter().try_rfold(r, |tail, e| -> Result<Reg> {
                    // f = fn(e tail)
                    let f = self.gen();
                    let x = self.gen_expr(e)?;
                    self.app(f, x, tail);
                    Ok(f)
                })?;
                self.expr(*f, f_reg)?;
            }
            Lam(prms, e) => {
                let (body, prm_users) = self.scoped(&prms, |c| c.gen_expr(*e))?;
                let mut prm_users = prm_users.collect::<Vec<_>>();

                if let Some(last_prm) = prm_users.pop() {
                    // Generate:
                    //   r = fn(x0 fn(x1 fn(x2 body)))
                    let tail = prm_users
                        .into_iter()
                        .try_fold(r, |tail, us| -> Result<Reg> {
                            let body = self.gen();
                            let x = self.materialize_users(us)?;
                            self.lam(x, body, tail);
                            Ok(body)
                        })?;
                    let x = self.materialize_users(last_prm)?;
                    self.lam(x, body, tail);
                } else {
                    // If there's no paramters, create a dummy one
                    let x = self.gen();
                    self.erase(x);
                    self.lam(x, body, r);
                }
            }
            If(e0, e1, e2) => {
                let c = self.gen_expr(*e0)?;
                let t = self.gen_expr(*e1)?;
                let f = self.gen_expr(*e2)?;
                self.branch(c, t, f, r);
            }
            Tup(mut es) => {
                if es.len() < 2 {
                    return Err(
                        "Invalid AST, invalid tuple, needs at least two elements".to_string()
                    );
                }
                // r = tup(e0 tup(e1 tup(e2 e3)))
                let last_elm = es.pop().unwrap();
                let tail = es.into_iter().try_fold(r, |acc, e| -> Result<Reg> {
                    // acc = tup(e r)
                    let r = self.gen();
                    let l = self.gen_expr(e)?;
                    self.tup(l, r, acc);
                    Ok(r)
                })?;
                self.expr(last_elm, tail)?;
            }
            Untup(xs, tup, body) => {
                if xs.len() < 2 {
                    return Err(
                        "Invalid AST, invalid un-tuple, needs at least two parameters".to_string(),
                    );
                }
                let (_, users) = self.scoped(&xs, |c| c.expr(*body, r))?;
                let mut users = users.collect::<Vec<_>>();
                let last_user = users.pop().unwrap();
                let last = self.materialize_users(last_user)?;
                let untup = users.into_iter().try_rfold(last, |r, us| -> Result<Reg> {
                    let res = self.gen();
                    let l = self.materialize_users(us)?;
                    self.tup(l, r, res);
                    Ok(res)
                })?;
                self.expr(*tup, untup)?;
            }
            Lift(expr) => todo!(),
            Lower(expr) => todo!(),
        }

        Ok(())
    }
}
