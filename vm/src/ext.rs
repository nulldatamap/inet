use bitflags::{bitflags, Flags};
use std::{
    alloc::Layout,
    any::TypeId,
    boxed::ThinBox,
    fmt,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::{DynMetadata, NonNull},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::rt::Rt;

// Like Arc, but:
// - No weak pointers
// - The pointer is always thin (so we can store arrays)
// - Type information is externalized to ExtTyDesc (i.e. layout, drop + vtable)
#[repr(align(16))]
struct CellInner<T> {
    ref_count: AtomicUsize,
    payload: T,
}

#[repr(transparent)]
pub struct UniqueCell<T>(NonNull<CellInner<T>>);

impl<T> UniqueCell<T> {
    fn new(data: T) -> Self {
        // SAFETY: Layout can't be zero because we've got the ref_counter
        let ptr: *mut CellInner<T> = unsafe { std::alloc::alloc(Self::layout()) }.cast();
        // SAFETY: ptr is valid and aligned because it's either:
        //         - Dangling (for ZST, which are always valid)
        //         - Freshly `alloc`ed and hence aligned and valid
        unsafe {
            ptr.write(CellInner {
                ref_count: AtomicUsize::new(1),
                payload: data,
            });
        }
        // SAFETY: Can't be null
        UniqueCell(unsafe { NonNull::new_unchecked(&raw mut *ptr) })
    }

    fn layout() -> Layout {
        Layout::new::<CellInner<T>>()
    }

    fn inner(&self) -> &CellInner<T> {
        unsafe { &*self.0.as_ptr() }
    }

    fn inner_mut(&mut self) -> &mut CellInner<T> {
        unsafe { &mut *self.0.as_ptr() }
    }

    fn into_shared(self) -> SharedCell {
        let c = SharedCell(self.0.cast());
        // Ownership is transferred to the untyped cell
        std::mem::forget(self);
        c
    }
}

impl<T> Deref for UniqueCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner().payload
    }
}

impl<T> DerefMut for UniqueCell<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner_mut().payload
    }
}

impl<T> Drop for UniqueCell<T> {
    fn drop(&mut self) {
        unsafe { std::ptr::drop_in_place::<T>(self.deref_mut()) };
        // SAFETY: Pointer and layout should be matching
        unsafe { std::alloc::dealloc(self.0.as_ptr().cast(), Self::layout()) };
    }
}

struct SharedCell(NonNull<CellInner<()>>);

impl SharedCell {
    // SAFETY: `self` must have come from a matching Cell<T>::into_untyped
    unsafe fn into_unique<T>(self) -> UniqueCell<T> {
        if self.inner().ref_count.load(Ordering::Acquire) != 1 {
            panic!("Trying to turn a SharedCell into a UniqueOnce, but there's still other references to it");
        }
        let c = UniqueCell(self.0.cast());
        // Ownership is transferred to the pointer
        std::mem::forget(self);
        c
    }

    // SAFETY: The user of the pointer must make sure the pointer is either
    //         never copied, or uses the reference counting API correctly
    unsafe fn to_ptr(self) -> NonNull<()> {
        let p = self.0.cast();
        // Ownership is transferred to the pointer
        std::mem::forget(self);
        p
    }

    // SAFETY: `ptr` must have come from UntypedCell::to_ptr
    unsafe fn from_raw(ptr: NonNull<CellInner<()>>) -> SharedCell {
        SharedCell(ptr)
    }

    fn inner(&self) -> &CellInner<()> {
        // SAFETY: This Cell being alive means that the inner pointer should still be valid
        unsafe { &*self.0.as_ptr() }
    }

    // SAFETY: The cell's contents must be of type T and the lifetime must be valid
    unsafe fn as_ref<'a, T>(&self) -> &'a T {
        // SAFETY: Because we have a &Cell then there's at least
        //         One reference keeping the inner cell alive
        //         So it's safe to create a ref to the payload
        let inner: &CellInner<T> = unsafe { self.0.cast().as_ref() };
        &inner.payload
    }

    fn dup(&self) -> SharedCell {
        self.inner().ref_count.fetch_add(1, Ordering::Relaxed);
        SharedCell(self.0)
    }

    // SAFETY: It's only safe to drop the inner cell value if there are
    //         No other references to it left
    unsafe fn drop_inner(self, vtable: DynMetadata<dyn Sync>) {
        // SAFETY: As long as type tags are valid, then the vtable should be valid
        //         For this pointer:
        let dropable_ptr: *mut dyn Sync = std::ptr::from_raw_parts_mut(self.0.as_ptr(), vtable);
        // SAFETY: CellInner drop_in_place should valid because the true type should match
        std::ptr::drop_in_place(dropable_ptr);
        // SAFETY: The pointer comes from alloc with the same layout, so it's safe to dealloc
        std::alloc::dealloc(self.0.as_ptr().cast(), vtable.layout());
        // We've already dropped the "true" value, so forget this one
        std::mem::forget(self);
    }

    fn erase(self, vtable: DynMetadata<dyn Sync>) {
        if self.inner().ref_count.fetch_sub(1, Ordering::Release) != 1 {
            // Nothing else to do, forget the value
            std::mem::forget(self);
            return;
        }

        let _ = self.inner().ref_count.load(Ordering::Acquire);
        unsafe {
            println!("DROPPING: {:?}", self.0.addr());
            // SAFETY: We're the last reference to this cell
            self.drop_inner(vtable);
        }
    }
}

#[cfg(debug_assertions)]
impl Drop for SharedCell {
    fn drop(&mut self) {
        panic!("UntypedCell is not allowed to be dropped")
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ExtTyFlags(u8);

bitflags! {
    impl ExtTyFlags: u8 {
        const CELL = 0b1;
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct ExtTy(u16);

impl ExtTy {
    const SHIFT: usize = 1;
    const FLAG_MASK: u16 = (1 << Self::SHIFT) - 1;

    // SAFETY: `idx` should refer to a valid ExtTyDesc and
    //         the flags should match the flags of said type descriptor
    const unsafe fn new(flags: ExtTyFlags, idx: u16) -> ExtTy {
        ExtTy(flags.bits() as u16 | (idx << Self::SHIFT))
    }

    fn flags(self) -> ExtTyFlags {
        let f = ExtTyFlags::from_bits_retain((self.0 & Self::FLAG_MASK) as u8);
        debug_assert!(!f.contains_unknown_bits());
        f
    }

    fn is_imm(self) -> bool {
        !self.flags().contains(ExtTyFlags::CELL)
    }

    fn is_cell(self) -> bool {
        self.flags().contains(ExtTyFlags::CELL)
    }

    fn index(&self) -> usize {
        (self.0 >> Self::SHIFT) as usize
    }
}

impl From<u16> for ExtTy {
    fn from(value: u16) -> Self {
        ExtTy(value)
    }
}

impl Into<u16> for ExtTy {
    fn into(self) -> u16 {
        self.0
    }
}

pub struct ExtTyDesc {
    name: String,
    flags: ExtTyFlags,
    index: usize,
    id: TypeId,
    vtable: DynMetadata<dyn Sync>,
}

impl ExtTyDesc {
    fn new<T: Sync + 'static>(name: impl ToString, flags: ExtTyFlags, index: usize) -> ExtTyDesc {
        let t_ptr = std::ptr::dangling::<CellInner<T>>();
        let dyn_ptr: *const dyn Sync = t_ptr;
        ExtTyDesc {
            name: name.to_string(),
            flags,
            index,
            id: TypeId::of::<T>(),
            vtable: std::ptr::metadata(dyn_ptr),
        }
    }
}

pub struct ExtVal<'h>(*mut (), PhantomData<&'h ()>);

impl<'h> ExtVal<'h> {
    const VAL_SHIFT: usize = 3;
    const TY_SHIFT: usize = 48;
    const VAL_MASK: usize = (1 << Self::TY_SHIFT) - 1;

    pub(crate) unsafe fn addr(&self) -> *mut () {
        self.0.map_addr(|x| x & Self::VAL_MASK)
    }

    pub fn ty(&self) -> ExtTy {
        ExtTy::from((self.0.addr() >> Self::TY_SHIFT) as u16)
    }

    pub(crate) unsafe fn raw(self) -> *mut () {
        let p = self.0;
        std::mem::forget(self);
        p
    }

    // SAFETY: Must have gotten `v` from ExtVal<'h>::addr()
    pub(crate) unsafe fn from_raw(v: *mut ()) -> ExtVal<'h> {
        ExtVal(v, PhantomData)
    }

    // SAFETY: Type/value invariants have to be respected and
    //         And val must be of lifetime 'h
    unsafe fn from_parts(ty: ExtTy, val: *mut ()) -> ExtVal<'h> {
        let v = ExtVal(
            val.map_addr(|x| x | (Into::<u16>::into(ty) as (usize) << Self::TY_SHIFT)),
            PhantomData,
        );
        debug_assert_eq!(v.addr(), val);
        debug_assert_eq!(v.ty(), ty);
        v
    }

    pub fn imm<T>(ty: ExtTy, val: T) -> ExtVal<'static>
    where
        T: Into<u32> + 'static,
    {
        debug_assert!(ty.is_imm());
        // SAFETY: The value is 'static and is an immidiate
        unsafe {
            ExtVal::from_parts(
                ty,
                std::ptr::null_mut::<()>().map_addr(|_| val.into() as (usize) << Self::VAL_SHIFT),
            )
        }
    }

    pub fn cell<T: 'h>(ty: ExtTy, val: T) -> ExtVal<'h> {
        debug_assert!(ty.is_cell());
        let cell = UniqueCell::new(val);
        // SAFETY: ExtVals are only ever cloned/dropped using the runtime
        //         Which respects reference counting
        let ptr = unsafe { cell.into_shared().to_ptr() };
        // SAFETY: The value is 'h and a cell type
        unsafe { ExtVal::from_parts(ty, ptr.as_ptr()) }
    }

    fn get_imm<T: From<u32>>(&self) -> T {
        debug_assert!(self.ty().is_imm());
        // SAFETY: It's safe to call addr() because immediates
        //         don't have to worry about ref-counting
        T::from(((unsafe { self.addr().addr() } >> Self::VAL_SHIFT) & 0xFFFFFFFF) as u32)
    }

    // SAFETY: The value must be a cell
    unsafe fn get_cell_ptr(&self) -> SharedCell {
        debug_assert!(self.ty().is_cell());
        // SAFETY: All cells should have been constructed via ExtVall::cell
        //         and therefore be non-null
        unsafe { SharedCell::from_raw(NonNull::new_unchecked(self.addr().cast())) }
    }

    pub fn dup(&self) -> Self {
        if self.ty().is_cell() {
            // SAFETY: It's a cell
            let c0 = unsafe { self.get_cell_ptr() };
            let c1 = c0.dup();
            // The TypedCell's ownership is still with &self, so don't drop it
            std::mem::forget(c0);
            // SAFETY: Inherits the validaty from self via dup
            unsafe { Self::from_parts(self.ty(), c1.to_ptr().as_ptr()) }
        } else {
            // Immediates are safe to blit:
            ExtVal(self.0, self.1)
        }
    }

    pub fn erase(self, exts: &Externals) {
        let ty = self.ty();
        if ty.is_cell() {
            let c = unsafe { self.get_cell_ptr() };
            // We've transfered ownership to `c`, so forget self
            std::mem::forget(self);
            c.erase(exts.type_desc_of(ty).vtable)
        }
    }

    pub fn is_truthy(&self) -> bool {
        // SAFETY: We're not using the value as a pointer
        unsafe { self.addr().addr() != 0 }
    }

    pub fn i32(x: i32) -> ExtVal<'h> {
        ExtVal::imm(Externals::I32_TY, x as u32)
    }

    pub fn get_i32(&self) -> i32 {
        self.get_imm::<u32>() as i32
    }
    // SAFETY: The type stored in the cell must be T
    pub(crate) unsafe fn get_ref_unchecked<T: Sync>(&self) -> &T {
        debug_assert!(self.ty().is_cell());
        // SAFETY: The lifetime is bound to the extval
        let p = self.get_cell_ptr();
        let r = p.as_ref();
        // `p` is still owned because of &self, so don't drop it
        std::mem::forget(p);
        r
    }

    // SAFETY: The type stored in the cell must be T
    pub(crate) unsafe fn get_unique_unchecked<T: Sync>(self) -> UniqueCell<T> {
        debug_assert!(self.ty().is_cell());
        // SAFETY: The lifetime is bound to the extval
        let p = self.get_cell_ptr();
        std::mem::forget(self);
        let r = p.into_unique();
        r
    }
}

impl<'h> fmt::Debug for ExtVal<'h> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.ty().is_imm() {
            write!(
                f,
                "<${:02X}:{:08X}>",
                self.ty().index(),
                self.get_imm::<u32>()
            )
        } else {
            write!(f, "<@{:02X}:{:08X}>", self.ty().index(), self.0.addr())
        }
    }
}

#[cfg(debug_assertions)]
impl<'h> Drop for ExtVal<'h> {
    fn drop(&mut self) {
        if self.ty().is_cell() {
            panic!("Did you forget to erase an ExtVal?");
        }
    }
}

type ExtFnF = for<'h> fn(&mut Rt<'h>, ExtVal<'h>, ExtVal<'h>) -> ExtVal<'h>;

pub struct Externals {
    pub extfns: Vec<ExtFnF>,
    ty_descs: Vec<ExtTyDesc>,
}

pub struct IoHandle {
    pub op_count: AtomicUsize,
}

impl Externals {
    pub const SEQ: u16 = 0;
    pub const ADD: u16 = 1;
    pub const PRINT: u16 = 2;

    pub const NIL_TY: ExtTy = unsafe { ExtTy::new(ExtTyFlags::empty(), 0) };
    pub const I32_TY: ExtTy = unsafe { ExtTy::new(ExtTyFlags::empty(), 1) };
    pub const IO_TY: ExtTy = unsafe { ExtTy::new(ExtTyFlags::CELL, 2) };

    pub fn builtins() -> Externals {
        Externals {
            extfns: vec![
                |_rt, l, _r| l,
                |_rt, l, r| {
                    assert_eq!(l.ty(), Self::I32_TY);
                    assert_eq!(r.ty(), Self::I32_TY);
                    ExtVal::i32(l.get_i32() + r.get_i32())
                },
                |rt, io, x| {
                    assert_eq!(io.ty(), Self::IO_TY);
                    println!("{:?}", x);
                    let io_handle = rt.get_cell::<IoHandle>(&io);
                    io_handle.op_count.fetch_add(1, Ordering::Relaxed);
                    rt.erase(x);
                    io
                },
            ],
            ty_descs: vec![
                ExtTyDesc::new::<u32>("nil", ExtTyFlags::empty(), 0),
                ExtTyDesc::new::<i32>("i32", ExtTyFlags::empty(), 1),
                ExtTyDesc::new::<IoHandle>("io", ExtTyFlags::CELL, 2),
            ],
        }
    }

    fn type_desc_of(&self, ty: ExtTy) -> &ExtTyDesc {
        let desc = &self.ty_descs[ty.index()];
        debug_assert_eq!(ty.flags(), desc.flags);
        desc
    }

    pub fn is_type<T: Sync + 'static>(&self, ty: ExtTy) -> bool {
        self.ty_descs[ty.index()].id == TypeId::of::<T>()
    }
}
