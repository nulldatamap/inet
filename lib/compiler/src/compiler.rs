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

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
struct ExtType {
    id: u16,
    name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
struct TypeId(usize);

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
enum UnboxedType {
    Fn(FnType),
    AnyExt,
    Ext(ExtType),
    Tup(Vec<TypeId>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeInfo {
    Unboxed(TypeId),
    Boxed(HashSet<TypeId>),
    Any,
}

#[derive(Copy, Clone)]
enum ConvAction {
    Box,
    Unbox(TypeId),
}

enum ConvFailure {
    TooNarrow(Option<HashSet<TypeId>>),
    Mismatch(TypeId, TypeId),
    CantPurify,
}

impl TypeInfo {
    fn is_boxed(&self) -> bool {
        match self {
            TypeInfo::Unboxed(..) => false,
            _ => true,
        }
    }

    fn unboxed_type(&self) -> Option<TypeId> {
        match self {
            TypeInfo::Unboxed(ut) => Some(*ut),
            _ => None,
        }
    }

    fn merge(&self, other: &TypeInfo) -> (TypeInfo, Option<ConvAction>) {
        use TypeInfo::*;

        match (self, other) {
            (Any, _) | (_, Any) => (Any, None),
            (Unboxed(t0), Unboxed(t1)) => (
                Boxed(HashSet::from([t0.clone(), t1.clone()])),
                Some(ConvAction::Box),
            ),
            (Unboxed(t0), Boxed(ts)) | (Boxed(ts), Unboxed(t0)) => {
                let mut ts = ts.clone();
                ts.insert(t0.clone());
                (Boxed(ts), Some(ConvAction::Box))
            }
            (Boxed(ts0), Boxed(ts1)) => (Boxed(ts0 | ts1), None),
        }
    }

    fn conversion(&self, to: &TypeInfo) -> result::Result<Option<ConvAction>, ConvFailure> {
        // If the types match, then there's no conversion needed
        if self == to {
            return Ok(None);
        }

        match to {
            TypeInfo::Boxed(tys) => {
                match self {
                    // Can't narrow the type of any
                    TypeInfo::Any => Err(ConvFailure::TooNarrow(None)),
                    TypeInfo::Boxed(self_tys) => {
                        // If we're widening the type then it's a no-op
                        if tys.is_subset(self_tys) {
                            Ok(None)
                        } else {
                            // If target type is not a super of the current type
                            // Then we can't convert
                            Err(ConvFailure::TooNarrow(Some(self_tys - tys)))
                        }
                    }
                    TypeInfo::Unboxed(ub) => {
                        // Same logic as above, but with a single element instead
                        if tys.contains(ub) {
                            Ok(None)
                        } else {
                            Err(ConvFailure::TooNarrow(Some(HashSet::from([*ub]))))
                        }
                    }
                }
            }
            TypeInfo::Any => {
                // Already boxed?
                if self.is_boxed() {
                    Ok(None)
                } else {
                    // No? Box it!
                    Ok(Some(ConvAction::Box))
                }
            }
            TypeInfo::Unboxed(ub) => {
                match self {
                    // We already checked f or equality, so they must mismatch
                    TypeInfo::Unboxed(self_ub) => Err(ConvFailure::Mismatch(*self_ub, *ub)),
                    TypeInfo::Boxed(self_tys) => {
                        // We're going from a singleton boxed type to the same type unboxed:
                        if self_tys.len() == 1 && self_tys.contains(ub) {
                            Ok(Some(ConvAction::Unbox(*ub)))
                        } else {
                            Err(ConvFailure::TooNarrow(Some(HashSet::from([*ub]))))
                        }
                    }
                    // Going from Any to anything else is too narrow
                    TypeInfo::Any => Err(ConvFailure::TooNarrow(None)),
                }
            }
        }
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

    fn unboxed(ti: TypeId) -> Metadata {
        Metadata {
            type_info: TypeInfo::Unboxed(ti),
            uses_io: false,
        }
    }

    fn merge(&self, other: &Self) -> (Metadata, Option<ConvAction>) {
        let (ti, ma) = self.type_info.merge(&other.type_info);
        (
            Metadata {
                uses_io: self.uses_io || other.uses_io,
                type_info: ti,
            },
            ma,
        )
    }

    fn conversion(&self, to: &Self) -> result::Result<Option<ConvAction>, ConvFailure> {
        if self.uses_io && !to.uses_io {
            return Err(ConvFailure::CantPurify);
        }

        self.type_info.conversion(&to.type_info)
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
    types: Vec<UnboxedType>,
    any_ty: TypeId,
}

impl TypeSystem {
    pub const NIL_TY: TypeId = TypeId(Externals::NIL_TY.index());
    pub const I32_TY: TypeId = TypeId(Externals::I32_TY.index());
    pub const IO_TY: TypeId = TypeId(Externals::IO_TY.index());

    fn new() -> TypeSystem {
        let mut types = Vec::new();
        let mut descs = Externals::ext_ty_descs();
        descs.sort_by_key(|d| d.index);
        for ty_desc in descs {
            types.push(UnboxedType::Ext(ExtType {
                id: ty_desc.index as u16,
                name: ty_desc.name,
            }))
        }
        let any = TypeId(types.len());
        types.push(UnboxedType::AnyExt);
        TypeSystem { types, any_ty: any }
    }

    fn any_extty(&self) -> TypeId {
        self.any_ty
    }

    fn extty(&self, ty: ExtTy) -> TypeId {
        let t = TypeId(ty.index());
        debug_assert!(match self.ty(t) {
            UnboxedType::Ext(ExtType { id: x, .. }) if ty.index() == *x as usize => true,
            _ => false,
        });
        t
    }

    fn ty(&self, ti: TypeId) -> &UnboxedType {
        &self.types[ti.0]
    }

    fn intern_ty(&mut self, ut: UnboxedType) -> TypeId {
        if let Some(idx) = self.types.iter().rposition(|x| *x == ut) {
            TypeId(idx)
        } else {
            let idx = self.types.len();
            self.types.push(ut);
            TypeId(idx)
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
            let tid = ts.intern_ty(UnboxedType::Fn(t.clone()));
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

    pub fn compile(e: Expr) -> Result<Node> {
        let mut c = Compiler::new();
        c.expr(e)
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

    fn perform_conv_action(&mut self, ma: ConvAction, n: Node) -> Hir {
        match ma {
            ConvAction::Box => {
                let ty = n.meta.type_info.unboxed_type().unwrap();
                Hir::Tup(vec![
                    // TODO: Use a value that is stable between Compiler instances
                    Hir::I32(ty.0 as i32),
                    n.hir,
                ])
            }
            ConvAction::Unbox(_) => {
                let box_ty = self.gen("__box_ty");
                let box_val = self.gen("__box_val");
                Hir::Untup(
                    vec![box_ty, box_val.clone()],
                    Box::new(n.hir),
                    Box::new(Hir::Var(box_val)),
                )
            }
        }
    }

    fn convert(&mut self, n: Node, to: Metadata) -> Result<Hir> {
        match n.meta.conversion(&to) {
            Err(err) => self.conv_failed(&n.meta, &to, err),
            Ok(None) => Ok(n.hir),
            Ok(Some(ca)) => Ok(self.perform_conv_action(ca, n)),
        }
    }

    fn conv_expr(&mut self, e: Expr, to: Metadata) -> Result<Hir> {
        let node = self.expr(e)?;
        self.convert(node, to)
    }

    pub(crate) fn expr(&mut self, e: Expr) -> Result<Node> {
        match e {
            Expr::Var(x) => self.var(x),
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
                                args.push(self.conv_expr(e, Metadata::unboxed(*prm))?);
                            }
                            return Ok(Node::new(Hir::ExtCall(id, args), Metadata::unboxed(ret)));
                        }
                        _ => {}
                    }
                }
                Ok(Node::new(
                    Hir::Call(
                        Box::new(self.conv_expr(*f, Metadata::any_io())?),
                        es.into_iter()
                            .map(|e| self.conv_expr(e, Metadata::any_io()))
                            .collect::<Result<Vec<_>>>()?,
                    ),
                    Metadata::any_io(),
                ))
            }
            Expr::Lit(l) => self.lit(l),
        }
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
                    Hir::Lam(names, Box::new(Hir::ExtCall(ext, args))),
                    Metadata::unboxed(fn_ty),
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
                    Metadata::unboxed(TypeSystem::I32_TY),
                ))
            }
            Literal::Num(n) => self.literal_out_of_range(n),
            Literal::Nil => Ok(Node::new(Hir::Nil, Metadata::unboxed(TypeSystem::NIL_TY))),
            Literal::True => Ok(Node::new(
                Hir::I32(1),
                Metadata::unboxed(TypeSystem::I32_TY),
            )),
            Literal::False => Ok(Node::new(
                Hir::I32(0),
                Metadata::unboxed(TypeSystem::I32_TY),
            )),
            Literal::Keyword(_) => todo!("keywords"),
            Literal::Symbol(_) => todo!("symbols"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;
    use crate::lowering::*;
    use crate::reader::read;
    use vm::ext::{ExtTy, Externals};

    fn t(i: ExtTy) -> TypeId {
        TypeId(i.index())
    }

    #[test]
    fn basics() {
        let i32md = Metadata::unboxed(TypeSystem::I32_TY);
        let nilmd = Metadata::unboxed(TypeSystem::NIL_TY);

        let mut compiler = Compiler::new();
        let arith_fn_ty = compiler.type_system.intern_ty(UnboxedType::Fn(FnType::new(
            TypeSystem::I32_TY,
            vec![TypeSystem::I32_TY, TypeSystem::I32_TY],
        )));

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
                Hir::Lam(
                    vec![Name::new("__arg", 1), Name::new("__arg", 2)],
                    Box::new(Hir::ExtCall(
                        Externals::I32_ADD,
                        vec![
                            Hir::Var(Name::new("__arg", 1)),
                            Hir::Var(Name::new("__arg", 2))
                        ]
                    ))
                ),
                Metadata::unboxed(arith_fn_ty)
            ))
        );
        assert!(c("(@i32add true nil)").is_err());
    }
}
