use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    num::NonZeroUsize,
};

use vm::{
    ext::{ExtVal, Externals},
    program::*,
    repr::{CombLabel, OperatorLabel, Port, Tag},
};

use crate::scope::Name;

type UInst = UnlinkedInst;

#[derive(Clone, PartialEq, Eq)]
pub enum NameOrRef {
    Name(Name),
    Ref(Name, Name),
}

impl std::fmt::Debug for NameOrRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NameOrRef::Name(n) => write!(f, "{:?}", n),
            NameOrRef::Ref(n0, n1) => write!(f, "ref({:?}:{:?})", n0, n1),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Hir {
    Nil,
    I32(i32),
    Var(Name),
    Add(Box<Hir>, Box<Hir>),
    If(Box<Hir>, Box<Hir>, Box<Hir>),
    Let(Name, Box<Hir>, Box<Hir>),
    Call(Box<Hir>, Box<Hir>),
    Lam(NameOrRef, Box<Hir>),
    Con(Name, Box<Hir>),
    CurryCall(Box<Hir>, Vec<Hir>),
    CurryLam(Vec<Name>, Box<Hir>),
    Ref(Name, Option<Name>),
    Tup(Vec<Hir>),
    Untup(Vec<NameOrRef>, Box<Hir>, Box<Hir>),
    ExtCall(u16, Vec<Hir>),
    Diverge(Box<Hir>),
    Do(Vec<Hir>),
    Lift(Box<Hir>),
    Lower(Box<Hir>),
}

impl std::fmt::Debug for Hir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Hir::Nil => f.write_str("nil"),
            Hir::I32(i) => write!(f, "{}", i),
            Hir::Var(x) => write!(f, "{:?}", x),
            Hir::Add(e0, e1) => write!(f, "({:?} + {:?})", e0, e1),
            Hir::If(e0, e1, e2) => write!(f, "(if {:?} then {:?} else {:?})", e0, e1, e2),
            Hir::Let(n, e0, e1) => write!(f, "(let {:?} = {:?} in {:?})", n, e0, e1),
            Hir::Call(e0, e1) => write!(f, "{:?}({:?})", e0, e1),
            Hir::Lam(n, e) => write!(f, "(\\{:?} -> {:?})", n, e),
            Hir::Con(n, e) => write!(f, "(\\{:?}* -> {:?})", n, e),
            Hir::CurryCall(hir, hirs) => todo!(),
            Hir::CurryLam(names, hir) => todo!(),
            Hir::Ref(e0, Some(e1)) => write!(f, "ref({:?} : {:?})", e0, e1),
            Hir::Ref(e0, None) => write!(f, "ref({:?} : _)", e0),
            Hir::Tup(es) => write!(
                f,
                "({})",
                es.iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Hir::Untup(xs, e0, e1) => write!(
                f,
                "(let ({}) = {:?} in {:?})",
                xs.iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join(", "),
                e0,
                e1
            ),
            Hir::ExtCall(i, es) => write!(
                f,
                "@{}({})",
                i,
                es.iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join(", "),
            ),
            Hir::Diverge(e) => write!(f, "diverge! {:?}", e),
            Hir::Do(es) => write!(
                f,
                "(do {})",
                es.iter()
                    .map(|e| format!("{:?}", e))
                    .collect::<Vec<_>>()
                    .join("; "),
            ),
            Hir::Lift(hir) => todo!(),
            Hir::Lower(hir) => todo!(),
        }
    }
}

pub fn i(i: i32) -> Hir {
    Hir::I32(i)
}
pub fn v(x: &'static str) -> Hir {
    Hir::Var(Name::global(x))
}
pub fn letv(x: &'static str, v: Hir, b: Hir) -> Hir {
    Hir::Let(Name::global(x), Box::new(v), Box::new(b))
}
pub fn extcall(lbl: u16, xs: Vec<Hir>) -> Hir {
    Hir::ExtCall(lbl, xs)
}
pub fn print(a: Hir, b: Hir) -> Hir {
    Hir::ExtCall(Externals::PRINT, vec![a, b])
}
pub fn lam(xs: Vec<&'static str>, b: Hir) -> Hir {
    Hir::CurryLam(xs.iter().map(|x| Name::global(x)).collect(), Box::new(b))
}
pub fn call(f: Hir, xs: Vec<Hir>) -> Hir {
    Hir::CurryCall(Box::new(f), xs)
}
pub fn if_(c: Hir, t: Hir, f: Hir) -> Hir {
    Hir::If(Box::new(c), Box::new(t), Box::new(f))
}
pub fn tup(es: Vec<Hir>) -> Hir {
    Hir::Tup(es)
}
pub fn untup(xs: Vec<&'static str>, tup: Hir, body: Hir) -> Hir {
    Hir::Untup(
        xs.iter()
            .map(|x| NameOrRef::Name(Name::global(x)))
            .collect::<Vec<_>>(),
        Box::new(tup),
        Box::new(body),
    )
}

pub fn def(x: &'static str, e: Hir) -> Def {
    Def {
        name: x.to_string(),
        body: e,
    }
}

#[derive(Debug)]
pub struct Def {
    name: String,
    body: Hir,
}

impl Def {
    pub fn new(name: String, body: Hir) -> Def {
        Def { name, body }
    }
}

type Scope = crate::scope::Scope<Name, Vec<Reg>>;

pub struct LowerSt {
    next_reg: NonZeroUsize,
    dup_count: u16,
    globals: HashSet<Name>,
    globals_refs: Vec<Name>,
    insts: Vec<UnlinkedInst>,
    scope: Scope,
    refs: HashMap<Name, Option<Reg>>,
}

type LoweringError = String;
type Result<T> = std::result::Result<T, LoweringError>;

impl LowerSt {
    fn new() -> LowerSt {
        LowerSt {
            next_reg: NonZeroUsize::new(1).unwrap(),
            globals: HashSet::new(),
            dup_count: 0,
            globals_refs: Vec::new(),
            insts: Vec::new(),
            scope: Scope::new(),
            refs: HashMap::new(),
        }
    }

    fn gen(&mut self) -> Reg {
        let r = Reg::new(self.next_reg);
        self.next_reg = self.next_reg.checked_add(1).expect("Register overflow");
        r
    }

    fn get_ref(&mut self, x: Name) -> Result<Reg> {
        if let Some(v) = self.refs.get_mut(&x) {
            if let Some(r) = v.take() {
                Ok(r)
            } else {
                return Err(format!("Ref `{}` used multiple times!", x));
            }
        } else {
            let r = self.gen();
            self.refs.insert(x, Some(r));
            Ok(r)
        }
    }

    fn nil(&mut self, r: Reg) {
        self.insts
            .push(UInst::Nilary(r, Port::from_extval(ExtVal::nil())));
    }

    fn gen_nil(&mut self) -> Reg {
        let r = self.gen();
        self.nil(r);
        r
    }

    fn val(&mut self, r: Reg, x: i32) {
        self.insts
            .push(UInst::Nilary(r, Port::from_extval(ExtVal::i32(x))));
    }

    fn gen_val(&mut self, x: i32) -> Reg {
        let r = self.gen();
        self.val(r, x);
        r
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

    fn global(&mut self, r: Reg, g: &Name) {
        if let Some(g) = self.globals_refs.iter().position(|x| x == g) {
            self.insts.push(UInst::Global(r, g));
        } else {
            self.insts.push(UInst::Global(r, self.globals_refs.len()));
            self.globals_refs.push(g.clone());
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

    fn refv(&mut self, r_in: Reg, r_out: Reg, res: Reg) {
        self.insts.push(UInst::Binary(
            Tag::Comb,
            CombLabel::Ref.into(),
            res,
            r_in,
            r_out,
        ));
    }

    pub fn lower(defs: Vec<Def>) -> Result<Vec<UnlinkedProgram>> {
        let mut c = LowerSt::new();

        // Declare all the globals:
        c.globals.extend(defs.iter().map(|d| Name::global(&d.name)));

        defs.into_iter().map(|d| c.def(d)).collect()
    }

    fn def(&mut self, def: Def) -> Result<UnlinkedProgram> {
        self.scope.enter();
        self.expr(def.body, Reg::ROOT)?;
        let prog = UnlinkedProgram {
            name: def.name,
            globals: self.globals_refs.drain(..).map(Name::into_string).collect(),
            reg_count: self.next_reg.get(),
            instructions: std::mem::replace(&mut self.insts, Vec::new()),
        };
        self.next_reg = NonZeroUsize::new(1).unwrap();
        self.scope.leave();
        debug_assert!(self.scope.is_empty());
        for (k, v) in self.refs.drain() {
            if let Some(v) = v {
                return Err(format!("Ref `{:?}`:{} wasn't used", v, k));
            }
        }
        Ok(prog)
    }

    fn gen_expr(&mut self, e: Hir) -> Result<Reg> {
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
        vars: &'a [NameOrRef],
        f: impl FnOnce(&mut Self) -> Result<T>,
    ) -> Result<(T, impl Iterator<Item = Result<Reg>> + 'a)> {
        for x in vars.iter() {
            match x {
                NameOrRef::Name(x) => self.scope.define(x.clone(), vec![]),
                _ => {}
            }
        }
        let res = f(self)?;
        let rs = vars.into_iter().map(|x| match x {
            NameOrRef::Name(x) => {
                let us = self.scope.undefine(&x);
                self.materialize_users(us)
            }
            NameOrRef::Ref(x_in, x_out) => {
                let r_ref = self.gen();
                let r_in = self.get_ref(x_in.clone())?;
                let r_out = self.get_ref(x_out.clone())?;
                self.refv(r_in, r_out, r_ref);
                Ok(r_ref)
            }
        });
        Ok((res, rs))
    }

    fn expr(&mut self, e: Hir, r: Reg) -> Result<()> {
        use Hir::*;
        println!(">> {:?} <- {:?}", r, e);

        match e {
            Nil => self.nil(r),
            I32(x) => self.val(r, x),
            Var(n) => {
                if let Some(users) = self.scope.lookup(&n) {
                    users.push(r);
                } else {
                    if self.globals.contains(&n) {
                        self.global(r, &n);
                    } else {
                        return Err(format!("Undefined variable `{}`", n));
                    }
                }
            }
            Let(x, v, b) => {
                let var_reg;
                {
                    let vars = &[NameOrRef::Name(x)];
                    let (_, mut var_regs) = self.scoped(vars, |c| c.expr(*b, r))?;
                    var_reg = var_regs.next().unwrap()?;
                }
                self.expr(*v, var_reg)?;
            }
            ExtCall(extfn, es) => {
                if es.len() <= 2 {
                    // Pass in the arguments directly
                    let mut es = es.into_iter();
                    let a = if let Some(e) = es.next() {
                        self.gen_expr(e)?
                    } else {
                        self.gen_nil()
                    };
                    let b = if let Some(e) = es.next() {
                        self.gen_expr(e)?
                    } else {
                        self.gen_nil()
                    };
                    self.extcall(extfn, a, b, r);
                } else {
                    // Make an arg value
                    let args = self.gen();
                    let rf = self.gen_val(extfn as i32);
                    let rn = self.gen_val(es.len() as i32 - 1);
                    self.extcall(Externals::MK_ARGS, rf, rn, args);
                    let mut es = es.into_iter();
                    let first = es.next().unwrap();
                    // r = push_arg(push_arg(push_arg(args x0) x1) x2)
                    let from = es.try_rfold(r, |to, e| -> Result<Reg> {
                        let from = self.gen();
                        let x = self.gen_expr(e)?;
                        self.extcall(Externals::ARGS_PUSH, from, x, to);
                        Ok(from)
                    })?;
                    let x = self.gen_expr(first)?;
                    self.extcall(Externals::ARGS_PUSH, args, x, from);
                }
            }
            Add(e0, e1) => {
                let a = self.gen_expr(*e0)?;
                let b = self.gen_expr(*e1)?;
                self.extcall(Externals::I32_ADD, a, b, r);
            }
            Call(f, e) => {
                let f = self.gen_expr(*f)?;
                let e = self.gen_expr(*e)?;
                self.app(f, e, r);
            }
            Lam(prm, e) => {
                let prms = &[prm];
                let (body, mut var_regs) = self.scoped(prms, |c| c.gen_expr(*e))?;
                let var_reg = var_regs.next().unwrap()?;
                debug_assert!(var_regs.next().is_none());
                drop(var_regs);
                self.lam(var_reg, body, r);
            }
            Con(x, e) => {
                let r_ref = self.get_ref(x)?;
                let body = self.gen_expr(*e)?;
                self.lam(r_ref, body, r);
            }
            CurryCall(..) | CurryLam(..) => todo!("Update or delete these"),
            /*
            CurryCall(f, mut es) => {
                if es.is_empty() {
                    // Call `f` with a dummy value
                    es.push(Hir::Nil);
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
            CurryLam(prms, e) => {
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
            */
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
                let (_, var_regs) = self.scoped(&xs, |c| c.expr(*body, r))?;
                let mut var_regs = var_regs.collect::<Result<Vec<_>>>()?;
                let last = var_regs.pop().unwrap();
                let untup = var_regs
                    .into_iter()
                    .try_rfold(last, |r, l| -> Result<Reg> {
                        let res = self.gen();
                        self.tup(l, r, res);
                        Ok(res)
                    })?;
                self.expr(*tup, untup)?;
            }
            Ref(x_in, x_out) => {
                let r_in = self.get_ref(x_in)?;
                let r_out = if let Some(x_out) = x_out {
                    self.get_ref(x_out)?
                } else {
                    let r = self.gen();
                    // self.erase(r);
                    self.nil(r);
                    r
                };
                self.refv(r_in, r_out, r);
            }
            Diverge(e) => {
                let r_div = self.gen_expr(*e)?;
                self.nil(r_div);
                self.erase(r);
            }
            Do(mut es) => {
                if let Some(last) = es.pop() {
                    for e in es {
                        let res = self.gen_expr(e)?;
                        self.nil(res);
                    }
                    self.expr(last, r)?;
                } else {
                    self.nil(r);
                }
            }
            Lift(expr) => todo!(),
            Lower(expr) => todo!(),
        }

        Ok(())
    }
}
