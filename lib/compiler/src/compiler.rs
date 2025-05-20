use std::collections::HashSet;
use std::result;

use vm::ext::{ExtTy, Externals};

use crate::ast::*;
use crate::lowering::Hir;
use crate::scope::{Name, Scope};

struct VarEntry {
    name: Name,
    type_info: TypeInfo,
}

struct BuiltinEntry {
    name: String,
    extcall_id: u16,
    arity: usize,
    ty: FnType,
    ty_id: TypeId,
    uses_io: bool,
}

impl BuiltinEntry {
    fn new(name: String, id: u16, fty: FnType, tid: TypeId) -> BuiltinEntry {
        let io = fty.uses_io();
        BuiltinEntry {
            name,
            extcall_id: id,
            arity: fty.prms.len(),
            ty: fty,
            ty_id: tid,
            uses_io: io,
        }
    }
}

enum Entry {
    Var(VarEntry),
    Builtin(BuiltinEntry),
}

impl Entry {
    fn name(&self) -> Name {
        match self {
            Entry::Var(VarEntry { name, .. }) => name.clone(),
            Entry::Builtin(BuiltinEntry { name, .. }) => Name::global(name),
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
struct ExtType {
    id: u16,
    name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
struct UnboxedTypeId(usize);

impl UnboxedTypeId {
    fn from_raw(v: usize) -> UnboxedTypeId {
        UnboxedTypeId(v)
    }

    fn index(&self) -> usize {
        self.0
    }
}

impl Into<TypeId> for UnboxedTypeId {
    fn into(self) -> TypeId {
        TypeId::from_raw(self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
struct TypeId(usize);

impl TypeId {
    const TAG_SHIFT: usize = size_of::<usize>() * 8 - 1;
    const TAG_MASK: usize = 1 << Self::TAG_SHIFT;

    pub const ANY: TypeId = TypeId(usize::MAX);

    fn from_raw(v: usize) -> TypeId {
        TypeId(v)
    }

    fn from_raw_parts(is_box: bool, v: usize) -> TypeId {
        TypeId(v | if is_box { Self::TAG_MASK } else { 0 })
    }

    fn is_box(&self) -> bool {
        (self.0 & Self::TAG_MASK) != 0
    }

    fn is_any(&self) -> bool {
        *self == Self::ANY
    }

    fn raw_index(&self) -> usize {
        self.0 & !Self::TAG_MASK
    }

    fn index(&self) -> Option<usize> {
        if *self == Self::ANY {
            None
        } else {
            Some(self.raw_index())
        }
    }

    fn as_unboxed(&self) -> Option<UnboxedTypeId> {
        if self.is_box() {
            None
        } else {
            Some(UnboxedTypeId::from_raw(self.raw_index()))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FnType {
    ret: TypeId,
    prms: Vec<TypeId>,
}

impl FnType {
    fn new(r: TypeId, ps: Vec<TypeId>) -> FnType {
        FnType { ret: r, prms: ps }
    }

    fn uses_io(&self) -> bool {
        self.prms.iter().any(|&prm| prm == TypeSystem::IO_TY)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum UnboxedType {}

impl UnboxedType {}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeInfo {
    AnyFn,
    Fn(FnType),
    AnyExt,
    Ext(ExtType),
    Tup(Vec<TypeId>),
    Boxed(HashSet<UnboxedTypeId>),
    Any,
}

#[derive(Debug, Copy, Clone)]
enum ConvAction {
    Box,
    Unbox(UnboxedTypeId),
}

#[derive(Debug)]
enum ConvFailure {
    TooNarrow(Option<HashSet<UnboxedTypeId>>),
    Mismatch(UnboxedTypeId, UnboxedTypeId),
    CantPurify,
}

impl TypeInfo {
    fn is_extty(&self) -> bool {
        match self {
            TypeInfo::AnyExt | TypeInfo::Ext(_) => true,
            _ => false,
        }
    }

    fn is_boxed(&self) -> bool {
        match self {
            TypeInfo::Boxed(_) | TypeInfo::Any => true,
            _ => false,
        }
    }

    fn is_unboxed(&self) -> bool {
        !self.is_boxed()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Metadata {
    type_info: TypeInfo,
    uses_io: bool,
}

impl Metadata {
    fn any_io() -> Metadata {
        Metadata {
            type_info: TypeInfo::Any,
            uses_io: true,
        }
    }

    fn pure(ti: TypeInfo) -> Metadata {
        Metadata {
            type_info: ti,
            uses_io: false,
        }
    }

    fn merge(&self, other: &Self, ts: &mut TypeSystem) -> Metadata {
        let ti = ts.merge(&self.type_info, &other.type_info);
        Metadata {
            uses_io: self.uses_io || other.uses_io,
            type_info: ti,
        }
    }

    fn conversion(
        &self,
        to: &Self,
        ts: &mut TypeSystem,
    ) -> result::Result<Option<ConvAction>, ConvFailure> {
        if self.uses_io && !to.uses_io {
            return Err(ConvFailure::CantPurify);
        }

        ts.conversion(&self.type_info, &to.type_info)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Node {
    hir: Hir,
    meta: Metadata,
}

impl Node {
    fn new(hir: Hir, md: Metadata) -> Node {
        Node { hir, meta: md }
    }
}

struct TypeSystem {
    types: Vec<TypeInfo>,
    any_extty: TypeId,
    any_fnty: TypeId,
    extty_count: usize,
}

impl TypeSystem {
    pub const NIL_TY: TypeId = TypeId(Externals::NIL_TY.index());
    pub const I32_TY: TypeId = TypeId(Externals::I32_TY.index());
    pub const IO_TY: TypeId = TypeId(Externals::IO_TY.index());

    pub const TYPE_TAG_SHIFT: usize = 16;

    fn new() -> TypeSystem {
        let mut types = Vec::new();
        let mut descs = Externals::ext_ty_descs();
        // TODO: Doesn't work for custom extty?
        descs.sort_by_key(|d| d.index);
        for ty_desc in descs {
            types.push(TypeInfo::Ext(ExtType {
                id: ty_desc.index as u16,
                name: ty_desc.name,
            }))
        }
        let extty_count = types.len();
        let any_extty = TypeId(types.len());
        types.push(TypeInfo::AnyExt);
        let any_fnty = TypeId(types.len());
        types.push(TypeInfo::AnyFn);
        TypeSystem {
            types,
            any_extty,
            any_fnty,
            extty_count,
        }
    }

    fn i32_ty(&self) -> &TypeInfo {
        self.ty(Self::I32_TY)
    }

    fn nil_ty(&self) -> &TypeInfo {
        self.ty(Self::NIL_TY)
    }

    fn extty_count(&self) -> usize {
        self.extty_count
    }

    fn any_extty(&self) -> TypeId {
        self.any_extty
    }

    fn any_fnty(&self) -> TypeId {
        self.any_fnty
    }

    fn extty(&self, ty: ExtTy) -> TypeId {
        let t = TypeId(ty.index());
        debug_assert!(match self.ty(t) {
            TypeInfo::Ext(ExtType { id: x, .. }) if ty.index() == *x as usize => true,
            _ => false,
        });
        t
    }

    fn uty(&self, ti: UnboxedTypeId) -> &TypeInfo {
        &self.types[ti.index()]
    }

    fn ty(&self, ti: TypeId) -> &TypeInfo {
        if let Some(i) = ti.index() {
            &self.types[i]
        } else {
            &TypeInfo::Any
        }
    }

    fn intern_ty(&mut self, ut: TypeInfo) -> TypeId {
        if let TypeInfo::Any = ut {
            return TypeId::ANY;
        }
        if let Some(idx) = self.types.iter().rposition(|x| *x == ut) {
            TypeId(idx)
        } else {
            let idx = self.types.len();
            self.types.push(ut);
            TypeId(idx)
        }
    }

    fn type_tag(&self, ty: &TypeInfo) -> i32 {
        use TypeInfo::*;
        match ty {
            AnyExt | AnyFn => panic!("Type `{:?}` is not taggable!", ty),
            Fn(fty) => self.any_fnty().0 as i32 | (fty.prms.len() as (i32) << Self::TYPE_TAG_SHIFT),
            Ext(ty) => ty.id as i32,
            Tup(_) => todo!("Tuple tagging"),
            _ => panic!("Tried to tag a boxed type"),
        }
    }

    fn merge(&mut self, t0: &TypeInfo, t1: &TypeInfo) -> TypeInfo {
        use TypeInfo::*;

        if t0 == t1 {
            return t0.clone();
        }

        match (t0, t1) {
            (Any, _) | (_, Any) => Any,
            (Boxed(ts0), Boxed(ts1)) => Boxed(ts0 | ts1),
            (t, Boxed(ts)) | (Boxed(ts), t) => {
                let mut ts = ts.clone();
                ts.insert(self.intern_ty(t.clone()).as_unboxed().unwrap());
                Boxed(ts)
            }
            (_, _) => Boxed(HashSet::from([
                self.intern_ty(t0.clone()).as_unboxed().unwrap(),
                self.intern_ty(t1.clone()).as_unboxed().unwrap(),
            ])),
        }
    }

    fn conversion(
        &mut self,
        from: &TypeInfo,
        to: &TypeInfo,
    ) -> result::Result<Option<ConvAction>, ConvFailure> {
        // If the types match, then there's no conversion needed
        if from == to {
            return Ok(None);
        }

        match to {
            TypeInfo::Any => {
                // Already boxed?
                if from.is_boxed() {
                    Ok(None)
                } else {
                    // No? Box it!
                    Ok(Some(ConvAction::Box))
                }
            }
            TypeInfo::Boxed(tys) => {
                if from == &TypeInfo::Any {
                    Err(ConvFailure::TooNarrow(None))
                } else if let TypeInfo::Boxed(from_tys) = from {
                    // If we're widening the type then it's a no-op
                    if tys.is_subset(from_tys) {
                        Ok(None)
                    } else {
                        // If target type is not a super of the current type
                        // Then we can't convert
                        Err(ConvFailure::TooNarrow(Some(from_tys - tys)))
                    }
                } else {
                    let tid = self.intern_ty(from.clone()).as_unboxed().unwrap();
                    // Same logic as above, but with a single element instead
                    if tys.contains(&tid) {
                        Ok(Some(ConvAction::Box))
                    } else {
                        Err(ConvFailure::TooNarrow(Some(HashSet::from([tid]))))
                    }
                }
            }
            _ => {
                let ub = self.intern_ty(to.clone()).as_unboxed().unwrap();
                match from {
                    // Going from Any to anything else is too narrow
                    TypeInfo::Any => Err(ConvFailure::TooNarrow(None)),
                    TypeInfo::Boxed(from_tys) => {
                        // We're going from a singleton boxed type to the same type unboxed:
                        if from_tys.len() == 1 && from_tys.contains(&ub) {
                            Ok(Some(ConvAction::Unbox(ub)))
                        } else {
                            Err(ConvFailure::TooNarrow(Some(HashSet::from([ub]))))
                        }
                    }
                    // We already checked f or equality, so they must mismatch
                    _ => Err(ConvFailure::Mismatch(
                        self.intern_ty(from.clone()).as_unboxed().unwrap(),
                        ub,
                    )),
                }
            }
        }
    }
}

struct Compiler {
    scope: Scope<String, Entry>,
    type_system: TypeSystem,
    next_name_id: usize,
}

type CompilerError = String;
type Result<T> = std::result::Result<T, CompilerError>;

impl Compiler {
    fn new() -> Compiler {
        let mut scope: Scope<String, Entry> = Scope::new();
        let mut ts = TypeSystem::new();
        let any_ = ts.any_extty();
        let i32_ = TypeSystem::I32_TY;
        let io_ = TypeSystem::IO_TY;

        scope.enter();
        let builtins = [
            (
                "@i32add",
                Externals::I32_ADD,
                FnType::new(i32_, vec![i32_, i32_]),
            ),
            (
                "@print",
                Externals::PRINT,
                FnType::new(io_, vec![io_, any_]),
            ),
        ];
        for (n, i, t) in builtins {
            let tid = ts.intern_ty(TypeInfo::Fn(t.clone()));
            scope.define(
                n.to_string(),
                Entry::Builtin(BuiltinEntry::new(n.to_string(), i, t, tid)),
            );
        }

        Compiler {
            scope,
            type_system: ts,
            next_name_id: 1,
        }
    }

    fn gen(&mut self, x: impl ToString) -> Name {
        let i = self.next_name_id;
        self.next_name_id += 1;
        Name::new(x, i)
    }

    pub fn compile(e: Expr) -> Result<Hir> {
        let mut c = Compiler::new();
        let boxed_r = c.conv_expr(e, &Metadata::any_io())?;
        Ok(c.unbox_node(boxed_r))
    }

    fn undefined_var<T>(&self, x: &str) -> Result<T> {
        Err(format!("Undefined variable: `{}`", x))
    }

    fn invalid_arity<T>(&self, f: &str, got: usize, exp: usize) -> Result<T> {
        Err(format!(
            "`{}` is being called with the wrong number of arguments. Expected {}, got {}",
            f, exp, got
        ))
    }

    fn literal_out_of_range<T>(&self, x: i64) -> Result<T> {
        Err(format!(
            "Integer value `{}` is too large to fix in a 32bit signed integer",
            x
        ))
    }

    fn conv_failed<T>(&self, from: &Metadata, to: &Metadata, failure: ConvFailure) -> Result<T> {
        let reason = match failure {
            ConvFailure::TooNarrow(None) => {
                format!("can't go from `any` to narrower type")
            }
            ConvFailure::TooNarrow(Some(tys)) => {
                let mut ty_names = tys.iter().map(|x| format!("{:?}", x)).collect::<Vec<_>>();
                ty_names.sort();
                format!(
                    "target type is too narrow, doesn't contain: {}",
                    ty_names.join(", ")
                )
            }
            ConvFailure::Mismatch(..) => {
                format!("they're mismatched")
            }
            ConvFailure::CantPurify => format!("can't turn a IO expression into a pure one"),
        };
        Err(format!(
            "Failed to convert from {:?} to {:?}: {}",
            from, to, reason
        ))
    }

    fn box_node(&mut self, n: Node) -> Hir {
        Hir::Tup(vec![
            // TODO: Use a value that is stable between Compiler instances
            Hir::I32(self.type_system.type_tag(&n.meta.type_info)),
            n.hir,
        ])
    }

    fn unbox_node(&mut self, v: Hir) -> Hir {
        let box_ty = self.gen("__box_ty");
        let box_val = self.gen("__box_val");
        Hir::Untup(
            vec![box_ty, box_val.clone()],
            Box::new(v),
            Box::new(Hir::Var(box_val)),
        )
    }

    fn perform_conv_action(&mut self, ma: ConvAction, n: Node) -> Hir {
        match ma {
            ConvAction::Box => self.box_node(n),
            ConvAction::Unbox(_) => self.unbox_node(n.hir),
        }
    }

    fn convert(&mut self, n: Node, to: &Metadata) -> Result<Hir> {
        match n.meta.conversion(&to, &mut self.type_system) {
            Err(err) => self.conv_failed(&n.meta, to, err),
            Ok(None) => Ok(n.hir),
            Ok(Some(ca)) => Ok(self.perform_conv_action(ca, n)),
        }
    }

    fn conv_expr(&mut self, e: Expr, to: &Metadata) -> Result<Hir> {
        let node = self.expr(e)?;
        self.convert(node, to)
    }

    pub(crate) fn expr(&mut self, e: Expr) -> Result<Node> {
        match e {
            Expr::Var(x) => self.var(x),
            Expr::Let(bs, mut es) => {
                let mut xs = Vec::new();
                // TODO: IO sequencing
                for b in bs {
                    let val = self.conv_expr(b.expr, &Metadata::any_io())?;
                    let b_name = self.gen(b.name.clone());
                    self.scope.define(
                        b.name.clone(),
                        Entry::Var(VarEntry {
                            name: b_name,
                            type_info: TypeInfo::Any,
                        }),
                    );
                    xs.push((b.name, val));
                }
                // TODO: IO sequencing
                let body = self.expr(es.pop().unwrap_or(Expr::Lit(Literal::Nil)))?;
                let Node { mut hir, meta } = body;
                for (n, x) in xs.into_iter().rev() {
                    hir = Hir::Let(self.scope.undefine(&n).name(), Box::new(x), Box::new(hir));
                }
                Ok(Node { hir, meta })
            }
            Expr::Invoke(f, es) => {
                if let Expr::Var(head) = &*f {
                    match self.scope.lookup(&head) {
                        Some(Entry::Builtin(BuiltinEntry {
                            extcall_id,
                            arity,
                            ty,
                            ..
                        })) => {
                            let arity = *arity;
                            let id = *extcall_id;
                            if arity != es.len() {
                                return self.invalid_arity(head, es.len(), arity);
                            }
                            let ret = ty.ret;
                            let mut args = Vec::new();
                            for (e, prm) in es.into_iter().zip(ty.prms.clone().iter()) {
                                args.push(self.conv_expr(
                                    e,
                                    &Metadata::pure(self.type_system.ty(*prm).clone()),
                                )?);
                            }
                            return Ok(Node::new(
                                Hir::ExtCall(id, args),
                                Metadata::pure(self.type_system.ty(ret).clone()),
                            ));
                        }
                        _ => {}
                    }
                }
                Ok(Node::new(
                    Hir::CurryCall(
                        Box::new(self.conv_expr(*f, &Metadata::any_io())?),
                        es.into_iter()
                            .map(|e| self.conv_expr(e, &Metadata::any_io()))
                            .collect::<Result<Vec<_>>>()?,
                    ),
                    Metadata::any_io(),
                ))
            }
            Expr::If(e0, e1, e2) => {
                // TODO: IO sequencing
                // TODO: Flow typing
                let cond = self.expr(*e0)?;
                let cond_val = match cond.meta.type_info {
                    TypeInfo::Boxed(ts) => {
                        // Same idea as above
                        if ts.iter().all(|t| !self.type_system.uty(*t).is_extty()) {
                            return self.expr(*e1);
                        }
                        // Otherwise we actually have to inspect the box type
                        // to safely construct a thruthy value
                        self.truth_of_expr(cond.hir, Some(ts))?
                    }
                    TypeInfo::Any => self.truth_of_expr(cond.hir, None)?,
                    ty => {
                        debug_assert!(ty.is_unboxed());
                        if !ty.is_extty() {
                            // We can't directly pass a tuple/fn to an `if`
                            // But they're always considered truthy anyway
                            // So just return the `then` branch
                            return self.expr(*e1);
                        }
                        cond.hir
                    }
                };
                let b_then = self.expr(*e1)?;
                let b_else = self.expr(*e2)?;
                let meta = b_then.meta.merge(&b_else.meta, &mut self.type_system);
                Ok(Node {
                    hir: Hir::If(
                        Box::new(cond_val),
                        Box::new(self.convert(b_then, &meta)?),
                        Box::new(self.convert(b_else, &meta)?),
                    ),
                    meta,
                })
            }
            Expr::Do(mut es) => {
                // TODO: IO Sequencing
                self.expr(es.pop().unwrap_or(Expr::Lit(Literal::Nil)))
            }
            /*
            Expr::Fn(ps, mut es) => {
                for p in &ps {
                    let name = self.gen(p.clone());
                    self.scope.define(
                        p.clone(),
                        Entry::Var(VarEntry {
                            name,
                            type_info: TypeInfo::Any,
                        }),
                    );
                }
                let Node{ hir: body, meta: ret_meta } = self.conv_expr(
                    es.pop().unwrap_or(Expr::Lit(Literal::Nil)),
                    &Metadata::any_io(),
                )?;
                let (prms_var, body) = if ps.len() == 1 {
                    (self.scope.undefine(&ps[0]).name(), body)
                } else {
                    let prms_var = self.gen("__prms");
                    (
                        prms_var,
                        Hir::Untup(
                            ps.into_iter()
                                .map(|p| self.scope.undefine(&p).name())
                                .collect::<Vec<_>>(),
                            Box::new(Hir::Var(prms_var)),
                            Box::new(body),
                        ),
                    )
                };
                let ret_ty = self.type_system.ty(ret_meta.type_info);
                Node::new(Hir::Lam(prms_var, Box::new(body)), Metadata {
                    type_info: TypeInfo::Unboxed(UnboxedType::Fn(ret_ty))
                    uses_io: ret_meta.uses_io,
                })
            }
            */
            Expr::Lit(l) => self.lit(l),
            _ => todo!(),
        }
    }

    fn truth_of_expr(&mut self, e: Hir, tys: Option<HashSet<UnboxedTypeId>>) -> Result<Hir> {
        // If all the possibly values `e` can be are all exttys then we can safely
        // pass it unboxed
        if tys
            .as_ref()
            .map(|tys| tys.iter().all(|t| self.type_system.uty(*t).is_extty()))
            .unwrap_or(false)
        {
            return Ok(self.unbox_node(e));
        }

        // ExtTys' truthiness is their unboxed value
        // While tuples and fns are always truthy
        let e_ty = self.gen("__box_ty");
        let e_val = self.gen("__box_val");
        // let (__box_ty, __box_val) = e
        // if __box_ty < EXTTY_COUNT
        // then __box_val
        // else true
        Ok(Hir::Untup(
            vec![e_ty.clone(), e_val.clone()],
            Box::new(e),
            Box::new(Hir::If(
                Box::new(Hir::ExtCall(
                    Externals::I32_LT,
                    vec![
                        Hir::Var(e_ty),
                        Hir::I32(self.type_system.extty_count() as i32),
                    ],
                )),
                Box::new(Hir::Var(e_val)),
                Box::new(Hir::I32(1)),
            )),
        ))
    }

    fn var(&mut self, x: String) -> Result<Node> {
        match self.scope.lookup(&x) {
            Some(Entry::Var(VarEntry { name, type_info })) => Ok(Node::new(
                Hir::Var(name.clone()),
                Metadata::pure(type_info.clone()),
            )),
            // If we're referring to a builtin in non-head position
            // Then wrap it in a lambda: @i32add => (fn [x0 x1] (@i32add x0 xy))
            Some(Entry::Builtin(BuiltinEntry {
                extcall_id,
                arity,
                ty_id,
                ..
            })) => {
                let ext = *extcall_id;
                let fn_ty = *ty_id;
                let mut names = Vec::new();
                for _ in 0..*arity {
                    names.push(self.gen("__arg"));
                }
                let args = names
                    .iter()
                    .map(|x| Hir::Var(x.clone()))
                    .collect::<Vec<_>>();

                Ok(Node::new(
                    Hir::CurryLam(names, Box::new(Hir::ExtCall(ext, args))),
                    Metadata::pure(self.type_system.ty(fn_ty).clone()),
                ))
            }
            None => self.undefined_var(&x),
        }
    }

    fn lit(&mut self, lit: Literal) -> Result<Node> {
        match lit {
            Literal::Num(n) if n >= i32::MIN as i64 && n <= i32::MAX as i64 => {
                // TODO: We can technically fit 45bit integers
                Ok(Node::new(
                    Hir::I32(n as i32),
                    Metadata::pure(self.type_system.i32_ty().clone()),
                ))
            }
            Literal::Num(n) => self.literal_out_of_range(n),
            Literal::Nil => Ok(Node::new(
                Hir::Nil,
                Metadata::pure(self.type_system.nil_ty().clone()),
            )),
            Literal::True => Ok(Node::new(
                Hir::I32(1),
                Metadata::pure(self.type_system.i32_ty().clone()),
            )),
            Literal::False => Ok(Node::new(
                Hir::I32(0),
                Metadata::pure(self.type_system.i32_ty().clone()),
            )),
            Literal::Keyword(_) => todo!("keywords"),
            Literal::Symbol(_) => todo!("symbols"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{num::NonZeroUsize, sync::Mutex};

    use super::*;
    use crate::ast::Expr;
    use crate::lowering::*;
    use crate::reader::read;
    use vm::{
        ext::{ExtTy, ExtVal, Externals},
        heap::Heap,
        program::link_programs,
        repr::Port,
        rt::Rt,
    };

    fn t(i: ExtTy) -> TypeId {
        TypeId(i.index())
    }

    #[test]
    fn basics() {
        let mut compiler = Compiler::new();

        let i32md = Metadata::pure(compiler.type_system.i32_ty().clone());
        let nilmd = Metadata::pure(compiler.type_system.nil_ty().clone());

        let arith_fn_ty = TypeInfo::Fn(FnType::new(
            TypeSystem::I32_TY,
            vec![TypeSystem::I32_TY, TypeSystem::I32_TY],
        ));

        let mut c = |src| compiler.expr(Expr::parse(&read(src).unwrap()).unwrap());

        assert_eq!(c("true"), Ok(Node::new(Hir::I32(1), i32md.clone())));
        assert_eq!(c("false"), Ok(Node::new(Hir::I32(0), i32md.clone())));
        assert_eq!(c("nil"), Ok(Node::new(Hir::Nil, nilmd.clone())));
        assert_eq!(c("3"), Ok(Node::new(Hir::I32(3), i32md.clone())));
        assert_eq!(
            c("(@i32add 1 2)"),
            Ok(Node::new(
                Hir::ExtCall(Externals::I32_ADD, vec![Hir::I32(1), Hir::I32(2)]),
                i32md.clone()
            ))
        );
        assert_eq!(
            c("(@i32add 0 (@i32add 1 2))"),
            Ok(Node::new(
                Hir::ExtCall(
                    Externals::I32_ADD,
                    vec![
                        Hir::I32(0),
                        Hir::ExtCall(Externals::I32_ADD, vec![Hir::I32(1), Hir::I32(2)])
                    ]
                ),
                i32md.clone()
            ))
        );
        assert_eq!(
            c("@i32add"),
            Ok(Node::new(
                Hir::CurryLam(
                    vec![Name::new("__arg", 1), Name::new("__arg", 2)],
                    Box::new(Hir::ExtCall(
                        Externals::I32_ADD,
                        vec![
                            Hir::Var(Name::new("__arg", 1)),
                            Hir::Var(Name::new("__arg", 2))
                        ]
                    ))
                ),
                Metadata::pure(arith_fn_ty)
            ))
        );
        assert!(c("(@i32add true nil)").is_err());
    }

    #[test]
    fn eval() {
        static TEST_RESULT: Mutex<ExtVal<'static>> = Mutex::new(ExtVal::nil());

        fn get_test_result() -> ExtVal<'static> {
            let mut result = TEST_RESULT.lock().unwrap();
            std::mem::replace(&mut *result, ExtVal::nil())
        }

        let h = Heap::new(NonZeroUsize::new(4096).unwrap());
        let mut exts = Externals::builtins();
        let ext_store_test_result = exts.extfns.len() as u16;
        exts.extfns.push(|rt, x, y| {
            let mut test_result = TEST_RESULT.lock().unwrap();
            rt.erase(std::mem::replace(
                &mut *test_result,
                ExtVal::imm(x.ty(), x.get_imm::<u32>()),
            ));
            y
        });
        let mut rt = Rt::new(&h, exts);

        let cases = [
            ("3", ExtVal::i32(3)),
            ("(@i32add 1 3)", ExtVal::i32(4)),
            ("(@i32add (@i32add 0 1) (@i32add 2 3))", ExtVal::i32(6)),
            ("(let [x 9 y x] y)", ExtVal::i32(9)),
            ("(let [x (@i32add 1 1) y x] y)", ExtVal::i32(2)),
            ("(let [x 1] (let [x x] x))", ExtVal::i32(1)),
            ("(if true 1 0)", ExtVal::i32(1)),
            ("(if false 1 0)", ExtVal::i32(0)),
            ("(if (@i32add 1 0) 1 0)", ExtVal::i32(1)),
            ("(if (@i32add 0 0) 1 0)", ExtVal::i32(0)),
            ("(if nil nil 0)", ExtVal::i32(0)),
            ("(if nil 0 nil)", ExtVal::nil()),
            ("(if (if true true false) 1 0)", ExtVal::i32(1)),
            ("(if (if true nil 1) 1 0)", ExtVal::i32(0)),
            ("(if (if false nil 1) 1 0)", ExtVal::i32(1)),
            ("(let [x 3] (if x 1 0))", ExtVal::i32(1)),
            ("(do 0 1 2 3)", ExtVal::i32(3)),
            ("(do 0)", ExtVal::i32(0)),
            ("(do)", ExtVal::nil()),
        ];

        for (i, (src, expected_val)) in cases.into_iter().enumerate() {
            let case_name = format!("test_case#{}", i);
            println!("=== {}\n{}\n\n", case_name, src);
            let test_body = Compiler::compile(Expr::parse(&read(src).unwrap()).unwrap()).unwrap();
            println!("{:?}\n\n", test_body);
            let unlinked_program = LowerSt::lower(vec![Def::new(
                case_name.clone(),
                Hir::ExtCall(ext_store_test_result, vec![test_body]),
            )])
            .unwrap();
            let case_program = link_programs(&unlinked_program[..]).unwrap();
            rt.link(
                Port::from_global(case_program.get(&case_name).unwrap()),
                Port::from_extval(ExtVal::nil()),
            );
            rt.normalize();
            let result = get_test_result();
            assert_eq!(result.ty(), expected_val.ty());
            assert_eq!(result.get_imm::<u32>(), expected_val.get_imm::<u32>());
        }
    }
}
