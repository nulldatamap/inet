use std::collections::HashSet;
use std::result;

use vm::ext::{ExtTy, Externals};

use crate::ast::*;
use crate::lowering::{Hir, NameOrRef};
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
        let io = fty.has_io;
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
    Global(VarEntry),
    Local(VarEntry),
    Builtin(BuiltinEntry),
}

impl Entry {
    fn name(&self) -> Name {
        match self {
            Entry::Global(VarEntry { name, .. }) => name.clone(),
            Entry::Local(VarEntry { name, .. }) => name.clone(),
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
    has_io: bool,
    ret: TypeId,
    prms: Vec<TypeId>,
}

impl FnType {
    fn new(r: TypeId, ps: Vec<TypeId>, io: bool) -> FnType {
        FnType {
            has_io: io,
            ret: r,
            prms: ps,
        }
    }

    fn pure(r: TypeId, ps: Vec<TypeId>) -> FnType {
        FnType {
            has_io: false,
            ret: r,
            prms: ps,
        }
    }

    fn io(r: TypeId, ps: Vec<TypeId>) -> FnType {
        FnType {
            has_io: true,
            ret: r,
            prms: ps,
        }
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
    fn new(ty: TypeInfo, io: bool) -> Metadata {
        Metadata {
            type_info: ty,
            uses_io: io,
        }
    }

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

    fn any_fn_tag(&self, arity: usize) -> i32 {
        self.any_fnty().0 as i32 | (arity as (i32) << Self::TYPE_TAG_SHIFT)
    }

    fn type_tag(&self, ty: &TypeInfo) -> i32 {
        use TypeInfo::*;
        match ty {
            AnyExt | AnyFn => panic!("Type `{:?}` is not taggable!", ty),
            Fn(fty) => self.any_fn_tag(fty.prms.len()),
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
    cur_io: Option<Name>,
}

type CompilerError = String;
type Result<T> = std::result::Result<T, CompilerError>;

impl Compiler {
    fn new() -> Compiler {
        let mut scope: Scope<String, Entry> = Scope::new();
        let mut ts = TypeSystem::new();
        let any_ = ts.any_extty();
        let nil_ = TypeSystem::NIL_TY;
        let i32_ = TypeSystem::I32_TY;
        let io_ = TypeSystem::IO_TY;

        scope.enter();
        let builtins = [
            (
                "@i32add",
                Externals::I32_ADD,
                FnType::pure(i32_, vec![i32_, i32_]),
            ),
            (
                "@print",
                Externals::PRINT,
                // "pure" because the io is explicit
                FnType::pure(io_, vec![io_, any_]),
            ),
        ];
        for (n, i, t) in builtins {
            let tid = ts.intern_ty(TypeInfo::Fn(t.clone()));
            scope.define(
                n.to_string(),
                Entry::Builtin(BuiltinEntry::new(n.to_string(), i, t, tid)),
            );
        }

        scope.define(
            "print".to_string(),
            Entry::Global(VarEntry {
                name: Name::global("print"),
                type_info: TypeInfo::Fn(FnType::io(nil_, vec![TypeId::ANY])),
            }),
        );

        Compiler {
            scope,
            type_system: ts,
            next_name_id: 1,
            cur_io: None,
        }
    }

    fn reset(&mut self) {
        self.next_name_id = 1;
        self.cur_io = None;
    }

    fn gen(&mut self, x: impl ToString) -> Name {
        let i = self.next_name_id;
        self.next_name_id += 1;
        Name::new(x, i)
    }

    fn next_io_ref(&mut self) -> Hir {
        let io_out = self.gen("__io");
        let io_in = self.cur_io.replace(io_out.clone()).expect("no cur_io set");
        Hir::Ref(Box::new(Hir::Var(io_in)), Some(Box::new(Hir::Var(io_out))))
    }

    pub fn compile(e: Expr) -> Result<Hir> {
        let mut c = Compiler::new();
        let io_in = c.gen("__io_in");
        c.cur_io = Some(io_in.clone());
        let boxed_r = c.conv_expr(e, &Metadata::any_io())?;
        let result = c.unbox_node(boxed_r);
        let io_out = c.cur_io.take().unwrap();
        let io_ref = c.gen("__io_ref");
        let con = Hir::Lam(NameOrRef::Ref(io_in, io_out), Box::new(result));
        Ok(con)
    }

    fn undefined_var<T>(&self, x: &str) -> Result<T> {
        Err(format!("Undefined variable: `{}`", x))
    }

    fn invalid_arity<T>(&self, f: Option<&str>, got: usize, exp: usize) -> Result<T> {
        let f_msg = if let Some(name) = f {
            format!("`{}`", name)
        } else {
            "Function".to_string()
        };
        Err(format!(
            "{} is being called with the wrong number of arguments. Expected {}, got {}",
            f_msg, exp, got
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

    fn uninvokable_value<T>(&self, ty: &TypeInfo) -> Result<T> {
        Err(format!("Can't invoke value of type `{:?}`", ty))
    }

    fn box_node(&mut self, n: Node) -> Hir {
        match &n.meta.type_info {
            // TODO: In theory, we should also just make a version of the function
            //       That returns a box on its own instead of wrapping the function
            TypeInfo::Fn(FnType { ret, .. }) if !ret.is_box() => {
                // (ANY_FN_ARITY#N, (fn [__prms] (RET_TY, (val __prms))))
                let prms = self.gen("__prms");
                Hir::Tup(vec![
                    Hir::I32(self.type_system.type_tag(&n.meta.type_info)),
                    Hir::Lam(
                        NameOrRef::Name(prms.clone()),
                        Box::new(Hir::Tup(vec![
                            Hir::I32(self.type_system.type_tag(self.type_system.ty(*ret))),
                            Hir::Call(Box::new(n.hir), Box::new(Hir::Var(prms))),
                        ])),
                    ),
                ])
            }
            TypeInfo::Any | TypeInfo::Boxed(_) => {
                panic!("Tried to box an already boxed node: {:?}", n)
            }
            TypeInfo::AnyExt | TypeInfo::AnyFn => {
                panic!("Tried to box a type-erased node: {:?}", n)
            }
            _ => {
                Hir::Tup(vec![
                    // TODO: Use a value that is stable between Compiler instances
                    Hir::I32(self.type_system.type_tag(&n.meta.type_info)),
                    n.hir,
                ])
            }
        }
    }

    fn unbox_node(&mut self, v: Hir) -> Hir {
        let box_ty = self.gen("__box_ty");
        let box_val = self.gen("__box_val");
        Hir::Untup(
            vec![NameOrRef::Name(box_ty), NameOrRef::Name(box_val.clone())],
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
                        Entry::Local(VarEntry {
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
                                return self.invalid_arity(Some(head), es.len(), arity);
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
                let Node {
                    hir: head,
                    meta: head_meta,
                } = self.expr(*f)?;

                // TODO: IO sequencing
                let (mut args, ret_ty, io) = match &head_meta.type_info {
                    // We have a concrete function type?
                    TypeInfo::Fn(fn_ty) => {
                        if fn_ty.prms.len() == es.len() {
                            (
                                es.into_iter()
                                    .zip(fn_ty.prms.iter())
                                    .map(|(e, ti)| {
                                        self.conv_expr(
                                            e,
                                            &Metadata::pure(self.type_system.ty(*ti).clone()),
                                        )
                                    })
                                    .collect::<Result<Vec<_>>>()?,
                                self.type_system.ty(fn_ty.ret).clone(),
                                fn_ty.has_io
                            )
                        } else {
                            return self.invalid_arity(None, es.len(), fn_ty.prms.len());
                        }
                    }
                    TypeInfo::Any | TypeInfo::Boxed(_) => {
                        // TODO: In theory we can get more specific based on the types in `Boxed(..)`
                        (
                            es.into_iter()
                                .map(|e| self.conv_expr(e, &Metadata::any_io()))
                                .collect::<Result<Vec<_>>>()?,
                            TypeInfo::Any,
                            true
                        )
                    }
                    TypeInfo::AnyFn => panic!("ICE: AnyFn in unboxed head position of invoke"),
                    t => {
                        return self.uninvokable_value(&t);
                    }
                };

                let arity = args.len();
                if io {
                    args.insert(0, self.next_io_ref());
                }

                let arg_count = args.len();
                let args = if args.len() == 0 {
                    Hir::Nil
                } else if args.len() == 1 {
                    args.pop().unwrap()
                } else {
                    Hir::Tup(args)
                };

                let result = if head_meta.type_info.is_boxed() {
                    let err = self.throw(Expr::Lit(Literal::Nil))?;
                    let head_ty = self.gen("__box_ty");
                    let head_val = self.gen("__box_val");
                    // let (__box_ty, __box_val) = head
                    // if __box_ty == ANY_FN_ARITY#arg_count
                    // then __box_val ..args
                    // else throw nil ; ERROR
                    Hir::Untup(
                        vec![
                            NameOrRef::Name(head_ty.clone()),
                            NameOrRef::Name(head_val.clone()),
                        ],
                        Box::new(head),
                        Box::new(Hir::If(
                            Box::new(Hir::ExtCall(
                                Externals::EQ,
                                vec![
                                    Hir::Var(head_ty),
                                    // Use arity and arg_count, since we don't count the io ref
                                    Hir::I32(self.type_system.any_fn_tag(arity)),
                                ],
                            )),
                            Box::new(Hir::Call(Box::new(Hir::Var(head_val)), Box::new(args))),
                            // TODO: Proper errors
                            Box::new(err.hir),
                        )),
                    )
                } else {
                    // We checked for non-fn unboxed types earlier, so we know it's a fn type here:
                    Hir::Call(Box::new(head), Box::new(args))
                };

                Ok(Node {
                    hir: result,
                    meta: Metadata::new(ret_ty, io),
                })
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
            Expr::Fn(ps, mut es) => {
                let arity = ps.len();
                for p in &ps {
                    let name = self.gen(p.clone());
                    self.scope.define(
                        p.clone(),
                        Entry::Local(VarEntry {
                            name,
                            type_info: TypeInfo::Any,
                        }),
                    );
                }
                let Node {
                    hir: body,
                    meta: ret_meta,
                } = self.expr(es.pop().unwrap_or(Expr::Lit(Literal::Nil)))?;
                let ps = ps
                    .into_iter()
                    .map(|p| self.scope.undefine(&p).name())
                    .collect::<Vec<_>>();
                let lam = self.make_lam(ps, body, ret_meta.uses_io());
                let fn_ty = TypeInfo::Fn(FnType::new(
                    self.type_system.intern_ty(ret_meta.type_info),
                    vec![TypeId::ANY; arity],
                    ret_meta.uses_io,
                ));
                Ok(Node::new(lam, Metadata::new(fn_ty, ret_meta.uses_io)))
            }
            Expr::Lit(l) => self.lit(l),
            _ => todo!(),
        }
    }

    fn make_lam(&mut self, mut ps: Vec<Name>, body: Hir, io: bool) -> Hir {
        let (prms_var, body) = if ps.len() == 0 {
            (self.gen("__dummy_prm"), body)
        } else if ps.len() == 1 {
            (ps.pop().unwrap(), body)
        } else {
            let prms_var = self.gen("__prms");
            (
                prms_var.clone(),
                Hir::Untup(
                    ps.into_iter()
                        .map(|p| NameOrRef::Name(p))
                        .collect::<Vec<_>>(),
                    Box::new(Hir::Var(prms_var)),
                    Box::new(body),
                ),
            )
        };
        Hir::Lam(NameOrRef::Name(prms_var), Box::new(body))
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
            vec![
                NameOrRef::Name(e_ty.clone()),
                NameOrRef::Name(e_val.clone()),
            ],
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
            Some(Entry::Local(e) | Entry::Global(e)) => Ok(Node::new(
                Hir::Var(e.name.clone()),
                Metadata::pure(e.type_info.clone()),
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

                let lam = self.make_lam(names, Hir::ExtCall(ext, args));

                Ok(Node::new(
                    lam,
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

    fn throw(&mut self, e: Expr) -> Result<Node> {
        let div = self.expr(e)?;
        Ok(Node::new(
            Hir::Diverge(Box::new(div.hir)),
            Metadata::any_io(),
        ))
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
        ext::{ExtTy, ExtVal, Externals, IoHandle},
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

        let arith_fn_ty = TypeInfo::Fn(FnType::pure(
            TypeSystem::I32_TY,
            vec![TypeSystem::I32_TY, TypeSystem::I32_TY],
        ));

        let any0_nil_fn_ty = TypeInfo::Fn(FnType::pure(TypeSystem::NIL_TY, vec![]));
        let any1_fn_ty = TypeInfo::Fn(FnType::pure(TypeId::ANY, vec![TypeId::ANY]));
        let any2_fn_ty = TypeInfo::Fn(FnType::pure(TypeId::ANY, vec![TypeId::ANY, TypeId::ANY]));

        let mut c = |src| {
            compiler.reset();
            compiler.expr(Expr::parse(&read(src).unwrap()).unwrap())
        };

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
                Hir::Lam(
                    NameOrRef::Name(Name::new("__prms", 3)),
                    Box::new(Hir::Untup(
                        vec![
                            NameOrRef::Name(Name::new("__arg", 1)),
                            NameOrRef::Name(Name::new("__arg", 2))
                        ],
                        Box::new(Hir::Var(Name::new("__prms", 3))),
                        Box::new(Hir::ExtCall(
                            Externals::I32_ADD,
                            vec![
                                Hir::Var(Name::new("__arg", 1)),
                                Hir::Var(Name::new("__arg", 2))
                            ]
                        ))
                    ))
                ),
                Metadata::pure(arith_fn_ty)
            ))
        );
        assert!(c("(@i32add true nil)").is_err());
        assert_eq!(
            c("(fn [] nil)"),
            Ok(Node::new(
                Hir::Lam(
                    NameOrRef::Name(Name::new("__dummy_prm", 1)),
                    Box::new(Hir::Nil)
                ),
                Metadata::pure(any0_nil_fn_ty)
            ))
        );
        assert_eq!(
            c("(fn [x] x)"),
            Ok(Node::new(
                Hir::Lam(
                    NameOrRef::Name(Name::new("x", 1)),
                    Box::new(Hir::Var(Name::new("x", 1)))
                ),
                Metadata::pure(any1_fn_ty)
            ))
        );
        assert_eq!(
            c("(fn [x y] x)"),
            Ok(Node::new(
                Hir::Lam(
                    NameOrRef::Name(Name::new("__prms", 3)),
                    Box::new(Hir::Untup(
                        vec![
                            NameOrRef::Name(Name::new("x", 1)),
                            NameOrRef::Name(Name::new("y", 2))
                        ],
                        Box::new(Hir::Var(Name::new("__prms", 3))),
                        Box::new(Hir::Var(Name::new("x", 1)))
                    ))
                ),
                Metadata::pure(any2_fn_ty)
            ))
        );
    }

    #[test]
    fn eval() {
        #[derive(Default)]
        struct TestResult {
            result: Option<ExtVal<'static>>,
            diverged: Option<ExtVal<'static>>,
        }

        impl TestResult {
            const fn new() -> TestResult {
                TestResult {
                    result: None,
                    diverged: None,
                }
            }
        }

        static TEST_RESULT: Mutex<TestResult> = Mutex::new(TestResult::new());

        fn get_test_result() -> result::Result<ExtVal<'static>, ExtVal<'static>> {
            let mut result = TEST_RESULT.lock().unwrap();
            let TestResult { result, diverged } =
                std::mem::replace(&mut *result, TestResult::new());
            match (result, diverged) {
                (Some(_), Some(_)) => panic!("Test both had a result and diverged(??)"),
                // TODO: (None, None) => panic!("Test both had no result, but didn't diverge(??)"),
                (None, None) => Err(ExtVal::nil()),
                (Some(x), _) => Ok(x),
                (_, Some(x)) => Err(x),
            }
        }

        let h = Heap::new(NonZeroUsize::new(4096).unwrap());
        let mut exts = Externals::builtins();
        let ext_store_test_result = exts.extfns.len() as u16;
        exts.extfns.push(|rt, x, y| {
            let mut test_result = TEST_RESULT.lock().unwrap();
            let v = ExtVal::imm(x.ty(), x.get_imm::<u32>());
            if let Some(x) = test_result.result.replace(v) {
                rt.erase(x);
                panic!("Test had multiple results!");
            }
            y
        });
        // print:
        //     fn(tup(ref(io_in io_out) x) nil)
        //     io_out = @print(io_in x)
        let print_prog = vm::program::UnlinkedProgram {
            name: "print".to_string(),
            globals: Vec::new(),
            reg_count: 7,
            instructions: {
                use std::num::NonZero;
                use vm::program::{Reg, UnlinkedInst::*};
                use vm::repr::{CombLabel, Port, Tag};
                [
                    (Tag::Comb, CombLabel::Fn, 0, 1, 2),
                    (Tag::Comb, CombLabel::Tup, 1, 3, 4),
                    (Tag::Comb, CombLabel::Ref, 3, 5, 6),
                    (Tag::ExtFn, CombLabel::Fn, 6, 5, 4),
                ]
                .into_iter()
                .map(|(t, l, a, b, c)| {
                    Binary(
                        t,
                        l.into(),
                        if a == 0 {
                            Reg::ROOT
                        } else {
                            Reg::new(NonZero::new(a).unwrap())
                        },
                        Reg::new(NonZero::new(b).unwrap()),
                        Reg::new(NonZero::new(c).unwrap()),
                    )
                })
                .chain([Nilary(
                    Reg::new(NonZero::new(2).unwrap()),
                    Port::from_extval(ExtVal::nil()),
                )])
                .collect::<Vec<_>>()
            },
        };
        let mut rt = Rt::new(&h, exts);

        let cases = [
            ("3", Ok(ExtVal::i32(3))),
            ("(@i32add 1 3)", Ok(ExtVal::i32(4))),
            ("(@i32add (@i32add 0 1) (@i32add 2 3))", Ok(ExtVal::i32(6))),
            ("(let [x 9 y x] y)", Ok(ExtVal::i32(9))),
            ("(let [x (@i32add 1 1) y x] y)", Ok(ExtVal::i32(2))),
            ("(let [x 1] (let [x x] x))", Ok(ExtVal::i32(1))),
            ("(if true 1 0)", Ok(ExtVal::i32(1))),
            ("(if false 1 0)", Ok(ExtVal::i32(0))),
            ("(if (@i32add 1 0) 1 0)", Ok(ExtVal::i32(1))),
            ("(if (@i32add 0 0) 1 0)", Ok(ExtVal::i32(0))),
            ("(if nil nil 0)", Ok(ExtVal::i32(0))),
            ("(if nil 0 nil)", Ok(ExtVal::nil())),
            ("(if (if true true false) 1 0)", Ok(ExtVal::i32(1))),
            ("(if (if true nil 1) 1 0)", Ok(ExtVal::i32(0))),
            ("(if (if false nil 1) 1 0)", Ok(ExtVal::i32(1))),
            ("(let [x 3] (if x 1 0))", Ok(ExtVal::i32(1))),
            ("(do 0 1 2 3)", Ok(ExtVal::i32(3))),
            ("(do 0)", Ok(ExtVal::i32(0))),
            ("(do)", Ok(ExtVal::nil())),
            ("((fn [] 1))", Ok(ExtVal::i32(1))),
            ("((fn [x] x) 1)", Ok(ExtVal::i32(1))),
            ("((fn [x y] y) 1 2)", Ok(ExtVal::i32(2))),
            ("(let [f (fn [x y] y)] (f 1 2))", Ok(ExtVal::i32(2))),
            (
                "(let [id (fn [x] x) f (fn [x y] y)] ((id f) 1 2))",
                Ok(ExtVal::i32(2)),
            ),
            ("((if true (fn [] 0) (fn [] nil)))", Ok(ExtVal::i32(0))),
            // TOOD: Fix once error handling has been addded
            (
                "(let [too-many (fn [] 1)] (too-many 1 2 3))",
                Err(ExtVal::nil()),
            ),
            (
                "(let [too-few (fn [x y z] 99)] (too-few))",
                Err(ExtVal::nil()),
            ),
            ("(let [not-a-fn 3] (not-a-fn))", Err(ExtVal::nil())),
            (
                "(let [x ((if false (fn [] 0) (fn [] (fn [] 7))))] (if x (x) 0))",
                Ok(ExtVal::i32(7)),
            ),
            (
                "(let [f (fn []) diverge! (fn [] (f 0))] (if true (diverge!) 1))",
                Err(ExtVal::nil()),
            ),
            (
                "(let [f (fn []) diverge! (fn [] (f 0))] (if false (diverge!) 1))",
                Ok(ExtVal::i32(1)),
            ),
            ("(print 3)", Ok(ExtVal::nil())),
        ];

        for (i, (src, expected_val)) in cases.into_iter().enumerate() {
            let case_name = format!("test_case#{}", i);
            println!("=== {}\n{}\n\n", case_name, src);
            let test_body = Compiler::compile(Expr::parse(&read(src).unwrap()).unwrap()).unwrap();
            let full_test = Hir::Lam(
                        NameOrRef::Name(Name::new("__io", 99)),
                        Box::new(Hir::ExtCall(
                            ext_store_test_result,
                            vec![Hir::Call(
                                Box::new(test_body),
                                Box::new(Hir::Ref(Box::new(Hir::Var(Name::new("__io", 99))), None)),
                            )],
                        )),
                    );
            println!("{:?}\n\n", full_test);
            let unlinked_program = LowerSt::lower(vec![
                // let test io = @store_test_result(<test body>(ref io))
                Def::new(
                    case_name.clone(),
                    full_test,
                ),
                Def::new("print".to_string(), Hir::Nil),
            ])
            .unwrap();
            let mut unlinked_program = unlinked_program.iter().collect::<Vec<_>>();
            unlinked_program.push(&print_prog);
            let case_program = link_programs(&unlinked_program[..]).unwrap();
            let io_handle = ExtVal::cell(
                Externals::IO_TY,
                IoHandle {
                    op_count: 0.into(),
                    on_erase: None,
                },
            );
            rt.link(
                Port::from_global(case_program.get(&case_name).unwrap()),
                Port::from_extval(io_handle),
            );
            rt.normalize();
            match (expected_val, get_test_result()) {
                (Ok(e), Ok(g)) | (Err(e), Err(g)) => {
                    assert_eq!(g.ty(), e.ty());
                    assert_eq!(g.get_imm::<u32>(), e.get_imm::<u32>());
                }
                (Ok(_), _) => panic!("Expected a result, but test diverged"),
                (Err(_), _) => panic!("Expected test to diverge"),
            }
        }
    }
}
