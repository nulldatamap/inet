use bitflags::{bitflags, Flags};
use std::{
    alloc::Layout,
    any::TypeId,
    fmt,
    marker::{PhantomData, Unsize},
    ops::{Deref, DerefMut},
    ptr::{metadata, DynMetadata, NonNull, Pointee},
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::rt::Rt;

#[repr(C, align(16))]
#[derive(Debug)]
struct CellHeader<M> {
    ref_count: AtomicUsize,
    metadata: M,
}

// rerpr(C) because the ref_count should always be the first field (and the payload the last)
// (Thanks -Zrandomize-layout)
#[repr(C, align(16))]
struct CellInner<T: Pointee + ?Sized> {
    header: CellHeader<<T as Pointee>::Metadata>,
    payload: T,
}

#[repr(transparent)]
#[derive(Debug)]
pub struct UniqueCell<T: Pointee + ?Sized>(
    NonNull<CellHeader<<T as Pointee>::Metadata>>,
    PhantomData<T>,
);

pub trait Tracked {
    // SAFETY: The value must not be used after this call nor may it be dropped
    //         Instead this serves as a Drop replacement
    unsafe fn erase_in_place(&mut self, ext: &Externals);
}

impl<'h> Tracked for [ExtVal<'h>] {
    unsafe fn erase_in_place(&mut self, ext: &Externals) {
        for v in self {
            v.erase_in_place(ext);
        }
    }
}

trait ExtVTable: Tracked {}
impl<T: ?Sized + Tracked> ExtVTable for UniqueCell<T> {}

macro_rules! prim_tracked {
    ($($ty:ty),*) => { $(impl Tracked for $ty { unsafe fn erase_in_place(&mut self, _exts: &Externals) {} })* }
}
prim_tracked!((), u8, i8, u16, i16, u32, i32, i64, u64, usize, isize);

impl<T> UniqueCell<T> {
    fn new(data: T) -> Self {
        // SAFETY: Layout can't be zero because we've got the ref_counter
        let ptr: *mut CellInner<T> = unsafe { std::alloc::alloc(Self::layout()) }.cast();
        // SAFETY: ptr is valid and aligned because it's freshly `alloc`ed and hence aligned and valid
        unsafe {
            ptr.write(CellInner {
                header: CellHeader {
                    ref_count: AtomicUsize::new(1),
                    metadata: metadata((&raw mut (*ptr).payload)),
                },
                payload: data,
            });
        }
        // SAFETY: Can't be null
        let u = UniqueCell::<T>(
            unsafe { NonNull::new_unchecked(&raw mut *ptr).cast() },
            PhantomData,
        );
        debug_assert_eq!(UniqueCell::<T>::layout(), u.recover_layout());
        u
    }

    fn layout() -> Layout {
        Layout::new::<CellInner<T>>()
    }
}

impl<T: ?Sized> UniqueCell<T> {
    fn cell_ptr(&self) -> *const CellInner<T> {
        // SAFETY: Unique cells should have be made from inner cell:
        unsafe {
            std::ptr::from_raw_parts::<CellInner<T>>(self.0.as_ptr(), (*self.0.as_ptr()).metadata)
        }
    }
    fn recover_layout(&self) -> Layout {
        // SAFETY: Due to constraints of unsized UniqueCell, this show always be valid:
        unsafe { Layout::for_value_raw::<CellInner<T>>(self.cell_ptr()) }
    }

    fn header(&self) -> &CellHeader<<T as Pointee>::Metadata> {
        unsafe { &*self.0.as_ptr() }
    }

    fn into_shared(self) -> SharedCell {
        let c = SharedCell(self.0.cast());
        // Ownership is transferred to the untyped cell
        std::mem::forget(self);
        c
    }
}

impl<T> UniqueCell<[T]> {
    fn array_layout(n: usize) -> Layout {
        Layout::new::<CellHeader<<[T] as Pointee>::Metadata>>()
            .extend(Layout::array::<T>(n).unwrap())
            .unwrap()
            .0
            .pad_to_align()
    }

    fn from_array<U: Unsize<[T]>>(data: U) -> Self {
        // SAFETY: See `UniqueCell::new`
        let n = {
            let data: &[T] = &data;
            data.len()
        };
        let ptr: *mut CellInner<U> = unsafe { std::alloc::alloc(Self::array_layout(n)) }.cast();
        // SAFETY: See `UniqueCell::new`
        unsafe {
            ptr.write(CellInner {
                header: CellHeader {
                    ref_count: AtomicUsize::new(1),
                    // NOTE: We're going from U (Sized) to T (Unsized)
                    metadata: metadata(&(*ptr).payload),
                },
                payload: data,
            });
        }
        // SAFETY: Can't be null
        let u = UniqueCell::<[T]>(
            unsafe { NonNull::new_unchecked(&raw mut *ptr).cast() },
            PhantomData,
        );

        debug_assert_eq!(UniqueCell::<U>::layout(), u.recover_layout());
        debug_assert_eq!(UniqueCell::<U>::layout(), Self::array_layout(n));
        u
    }

    fn new_array(n: usize, mut f: impl FnMut() -> T) -> Self {
        // SAFETY: See `UniqueCell::new`
        let ptr: *mut CellInner<[T; 0]> =
            unsafe { std::alloc::alloc(Self::array_layout(n)) }.cast();
        // SAFETY: The header field should be valid from the alloc
        unsafe {
            (&raw mut (*ptr).header)
                .cast::<CellHeader<<[T] as Pointee>::Metadata>>()
                .write(CellHeader {
                    ref_count: AtomicUsize::new(1),
                    metadata: n,
                });
        };
        // Initialize the data
        let data = (&raw mut (unsafe { &mut *ptr }).payload).cast::<T>();
        for i in 0..(n as isize) {
            unsafe { data.offset(i).write(f()) };
        }

        // SAFETY: Can't be null
        let u = UniqueCell::<[T]>(
            unsafe { NonNull::new_unchecked(&raw mut *ptr).cast() },
            PhantomData,
        );

        debug_assert_eq!(Self::array_layout(n), u.recover_layout());
        u
    }
}

impl<T: ?Sized> Deref for UniqueCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: We get a thin pointer to the start of the payload
        //         Then using the metadata stored when we created the cell
        //         we can safely reconstiute a (possibly) fat pointer (in case of a DST)
        unsafe {
            std::ptr::from_raw_parts::<Self::Target>(
                &raw const (*(self.0.as_ptr() as *const CellInner<()>)).payload,
                (*self.0.as_ptr()).metadata,
            )
            .as_ref_unchecked()
        }
    }
}

impl<T: ?Sized> DerefMut for UniqueCell<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: See `deref`
        unsafe {
            std::ptr::from_raw_parts_mut::<Self::Target>(
                &raw mut ((*(self.0.as_ptr() as *mut CellInner<()>)).payload),
                (*self.0.as_ptr()).metadata,
            )
            .as_mut_unchecked()
        }
    }
}

impl<T: ?Sized> Drop for UniqueCell<T> {
    fn drop(&mut self) {
        panic!("UniqueCell is not allowed to be dropped. Did you forget to call erase on it? ");
    }
}

impl<T: ?Sized + Tracked> Tracked for UniqueCell<T> {
    unsafe fn erase_in_place(&mut self, exts: &Externals) {
        println!("Erasing UniqueCell: {:08X}", self.0.as_ptr().addr());
        unsafe { self.deref_mut().erase_in_place(exts) };
        // SAFETY: Pointer and layout should be matching
        unsafe { std::alloc::dealloc(self.0.as_ptr().cast(), self.recover_layout()) };
    }
}

#[repr(transparent)]
struct SharedCell(NonNull<CellHeader<()>>);

impl SharedCell {
    // SAFETY: `self` must have come from a matching Cell<T>::into_untyped
    unsafe fn into_unique<T>(self) -> UniqueCell<T> {
        if self.header().ref_count.load(Ordering::Acquire) != 1 {
            panic!("Trying to turn a SharedCell into a UniqueOnce, but there's still other references to it");
        }
        let c = UniqueCell(self.0.cast(), PhantomData);
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
    unsafe fn from_raw(ptr: NonNull<CellHeader<()>>) -> SharedCell {
        SharedCell(ptr)
    }

    fn header(&self) -> &CellHeader<()> {
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
        self.header().ref_count.fetch_add(1, Ordering::Relaxed);
        SharedCell(self.0)
    }

    // SAFETY: It's only safe to drop the inner cell value if there are
    //         No other references to it left
    unsafe fn erase_inner(mut self, vtable: DynMetadata<dyn ExtVTable>, exts: &Externals) {
        println!("DROP INNER: {:08X}", self.0.addr().get());
        // SAFETY: As long as type tags are valid, then the vtable should be valid
        //         For this pointer:
        let dropable_ptr: *mut dyn ExtVTable = std::ptr::from_raw_parts_mut(&raw mut self, vtable);
        // SAFETY: Shared cells should have be made from UniqueCell, so this should be valid
        dropable_ptr.as_mut_unchecked().erase_in_place(exts);
        // We've already dropped the "true" value, so forget these ones
        std::mem::forget(self);
    }

    fn erase(self, vtable: DynMetadata<dyn ExtVTable>, exts: &Externals) {
        println!("ERASE: {:08X}", self.0.addr().get());
        if self.header().ref_count.fetch_sub(1, Ordering::Release) != 1 {
            // Nothing else to do, forget the value
            std::mem::forget(self);
            return;
        }

        let _ = self.header().ref_count.load(Ordering::Acquire);
        unsafe {
            // SAFETY: We're the last reference to this cell
            self.erase_inner(vtable, exts);
        }
    }
}

impl Tracked for SharedCell {
    unsafe fn erase_in_place(&mut self, exts: &Externals) {}
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
    vtable: DynMetadata<dyn ExtVTable>,
}

impl ExtTyDesc {
    fn new<T: 'static + ?Sized + Pointee + Tracked>(
        name: impl ToString,
        flags: ExtTyFlags,
        index: usize,
    ) -> ExtTyDesc {
        let t_ptr: *const UniqueCell<T> = std::ptr::dangling();
        let dyn_ptr: *const dyn ExtVTable = t_ptr;
        ExtTyDesc {
            name: name.to_string(),
            flags,
            index,
            id: TypeId::of::<T>(),
            vtable: std::ptr::metadata(dyn_ptr),
        }
    }
}

#[repr(transparent)]
pub struct ExtVal<'h>(*mut (), PhantomData<&'h ()>);

// SAFETY: ExtVal is effective a enum { Imm(u32), Cell(SharedCell) }
//         And SharedCell is effectively an Arc (which are all sync)
//         So ExtVal should be Sync too
unsafe impl<'h> Sync for ExtVal<'h> {}

impl<'h> ExtVal<'h> {
    const VAL_SHIFT: usize = 3;
    const TY_SHIFT: usize = 48;
    const VAL_MASK: usize = (1 << Self::TY_SHIFT) - 1;

    pub fn nil() -> ExtVal<'static> {
        ExtVal(std::ptr::null_mut(), PhantomData)
    }

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

    pub fn arr_from<T: 'h, const N: usize>(ty: ExtTy, vals: [T; N]) -> ExtVal<'h> {
        debug_assert!(ty.is_cell());
        let cell = UniqueCell::from_array(vals);
        // SAFETY: See `cell()`
        let ptr = unsafe { cell.into_shared().to_ptr() };
        unsafe { ExtVal::from_parts(ty, ptr.as_ptr()) }
    }

    pub fn arr<T: 'h>(ty: ExtTy, n: usize, f: impl FnMut() -> T) -> ExtVal<'h> {
        debug_assert!(ty.is_cell());
        let cell = UniqueCell::new_array(n, f);
        // SAFETY: See `cell()`
        let ptr = unsafe { cell.into_shared().to_ptr() };
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

    pub fn erase(mut self, exts: &Externals) {
        // SAFETY: Because we're forgetting self afterwards, it's safe to erase in place
        unsafe { self.erase_in_place(exts) }
        std::mem::forget(self);
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

impl<'h> Tracked for ExtVal<'h> {
    unsafe fn erase_in_place(&mut self, exts: &Externals) {
        let ty = self.ty();
        if ty.is_cell() {
            let c = unsafe { self.get_cell_ptr() };
            c.erase(exts.type_desc_of(ty).vtable, exts)
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

impl Tracked for IoHandle {
    unsafe fn erase_in_place(&mut self, _exts: &Externals) {}
}

impl Externals {
    pub const SEQ: u16 = 0;
    pub const ADD: u16 = 1;
    pub const PRINT: u16 = 2;

    pub const NIL_TY: ExtTy = unsafe { ExtTy::new(ExtTyFlags::empty(), 0) };
    pub const I32_TY: ExtTy = unsafe { ExtTy::new(ExtTyFlags::empty(), 1) };
    pub const IO_TY: ExtTy = unsafe { ExtTy::new(ExtTyFlags::CELL, 2) };
    pub const ARR_TY: ExtTy = unsafe { ExtTy::new(ExtTyFlags::CELL, 3) };

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
                |rt, len, v| {
                    assert_eq!(len.ty(), Self::I32_TY);
                    let arr = if v.ty() == Self::NIL_TY {
                        // Fast path for nil, no need to go through dup
                        ExtVal::arr(Self::ARR_TY, len.get_imm::<u32>() as usize, || {
                            ExtVal::nil()
                        })
                    } else {
                        ExtVal::arr(Self::ARR_TY, len.get_imm::<u32>() as usize, || v.dup())
                    };
                    rt.erase(v);
                    arr
                },
            ],
            ty_descs: vec![
                ExtTyDesc::new::<u32>("nil", ExtTyFlags::empty(), 0),
                ExtTyDesc::new::<i32>("i32", ExtTyFlags::empty(), 1),
                ExtTyDesc::new::<IoHandle>("io", ExtTyFlags::CELL, 2),
                ExtTyDesc::new::<[ExtVal]>("arr", ExtTyFlags::CELL, 3),
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
