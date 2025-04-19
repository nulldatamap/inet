use core::fmt;
use std::sync::atomic::{AtomicPtr, Ordering::Relaxed};
use std::{marker::PhantomData, ptr::NonNull};

use crate::program::Program;

pub struct Wire<'h>(&'h Word);

impl<'h> Wire<'h> {
    pub fn to_ref(self) -> &'h Word {
        self.0
    }

    pub fn to_ptr(self) -> NonNull<Word> {
        // Use when stable: NonNull::from_ref(self.0)
        // SAFETY: Pointers from refs can't be null
        unsafe { NonNull::new_unchecked((self.0 as *const Word).cast_mut()) }
    }

    pub fn from_ref(r: &'h Word) -> Wire<'h> {
        Wire(r)
    }

    pub fn load(&self) -> Option<Port<'h>> {
        // SAFETY: The loaded value is of lifetime `h` because the Wire is
        unsafe { Port::option_from_raw(self.0.load()) }
    }

    pub fn swap(&self, p: Option<Port<'h>>) -> Option<Port<'h>> {
        // SAFETY: The loaded value is of lifetime `h` because the Wire is
        unsafe {
            Port::option_from_raw(
                self.0
                    .swap(p.map(|x| x.raw().as_ptr()).unwrap_or(std::ptr::null_mut())),
            )
        }
    }

    pub fn store(&self, p: Option<Port<'h>>) {
        self.0
            .store(p.map(|x| x.raw().as_ptr()).unwrap_or(std::ptr::null_mut()))
    }
}

#[repr(transparent)]
pub struct Word(AtomicPtr<()>);

impl Word {
    pub const NULL: Word = Word(AtomicPtr::new(std::ptr::null_mut()));

    pub fn store(&self, v: *mut ()) {
        self.0.store(v, Relaxed);
    }

    pub fn load(&self) -> *mut () {
        self.0.load(Relaxed)
    }

    pub fn swap(&self, v: *mut ()) -> *mut () {
        self.0.swap(v, Relaxed)
    }

    pub fn map_addr(self, f: impl FnOnce(usize) -> usize) -> Self {
        Word(AtomicPtr::new(self.load().map_addr(f)))
    }

    pub fn addr(self) -> usize {
        self.load().addr()
    }
}

impl Default for Word {
    fn default() -> Self {
        Word::NULL
    }
}

impl Clone for Word {
    fn clone(&self) -> Self {
        Word(AtomicPtr::new(self.0.load(Relaxed)))
    }
}

#[repr(align(16))]
#[derive(Default, Clone)]
pub struct WordPair(pub(crate) Word, pub(crate) Word);

const TAG_MASK: usize = 0b111;
const TAG_SHIFT: usize = 3;
const LABEL_SHIFT: usize = 48;
const ADDR_MASK: usize = !(TAG_MASK | 0xFFFFusize << LABEL_SHIFT);

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Tag {
    ExtVal = 1,
    ExtFn = 2,
    Global = 3,
    Comb = 4,
    Operator = 5,
    Wire = 6,
    Eraser = 7,
}

impl fmt::Debug for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tag::ExtVal => write!(f, "EXTVAL"),
            Tag::ExtFn => write!(f, "EXTFN"),
            Tag::Global => write!(f, "G"),
            Tag::Comb => write!(f, "C"),
            Tag::Operator => write!(f, "O"),
            Tag::Wire => write!(f, "W"),
            Tag::Eraser => write!(f, "_"),
        }
    }
}

impl Tag {
    fn is_binary(self) -> bool {
        matches!(self, Tag::Comb | Tag::Operator | Tag::ExtFn)
    }

    fn is_nilary(self) -> bool {
        !self.is_binary()
    }

    fn has_addr(self) -> bool {
        !matches!(self, Tag::Eraser)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum OperatorLabel {
    Branch,
    Lift,
    Lower,
}

impl Into<u16> for OperatorLabel {
    fn into(self) -> u16 {
        self as u16
    }
}

impl From<u16> for OperatorLabel {
    fn from(x: u16) -> Self {
        match x {
            0 => OperatorLabel::Branch,
            1 => OperatorLabel::Lift,
            2 => OperatorLabel::Lower,
            _ => panic!("Invalid operator label: `{}`", x),
        }
    }
}

impl fmt::Debug for OperatorLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            OperatorLabel::Branch => "?",
            OperatorLabel::Lift => "^",
            OperatorLabel::Lower => ",",
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum CombLabel {
    Fn,
    Tup,
    Dup(u16),
}

impl Into<u16> for CombLabel {
    fn into(self) -> u16 {
        match self {
            CombLabel::Fn => 0,
            CombLabel::Tup => 1,
            CombLabel::Dup(v) => {
                debug_assert!((v as usize + 2) <= u16::MAX as usize);
                2 + v
            }
        }
    }
}

impl From<u16> for CombLabel {
    fn from(x: u16) -> CombLabel {
        match x {
            0 => CombLabel::Fn,
            1 => CombLabel::Tup,
            _ => CombLabel::Dup(x - 2),
        }
    }
}

impl fmt::Debug for CombLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CombLabel::Fn => write!(f, "fn"),
            CombLabel::Tup => write!(f, "tup"),
            CombLabel::Dup(x) => write!(f, "dup{}", x),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct ExtFnLabel(u16);

impl ExtFnLabel {
    const FLIP_BIT: u16 = 1 << 15;

    pub fn new(flipped: bool, index: usize) -> Self {
        assert!(index <= (Self::FLIP_BIT as usize - 1));
        ExtFnLabel(if flipped {
            Self::FLIP_BIT | (index as u16)
        } else {
            index as u16
        })
    }

    pub fn is_flipped(self) -> bool {
        (self.0 & Self::FLIP_BIT) != 0
    }

    pub fn flip(self) -> Self {
        ExtFnLabel(self.0 ^ Self::FLIP_BIT)
    }

    pub fn get(self) -> (bool, usize) {
        (
            self.0 & Self::FLIP_BIT != 0,
            (self.0 & !Self::FLIP_BIT) as usize,
        )
    }
}

impl From<u16> for ExtFnLabel {
    fn from(x: u16) -> ExtFnLabel {
        ExtFnLabel(x)
    }
}

impl Into<u16> for ExtFnLabel {
    fn into(self) -> u16 {
        self.0
    }
}

impl fmt::Debug for ExtFnLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (fl, i) = self.get();
        if fl { write!(f, "'")?; }
        write!(f, "{}", i)
    }
}

pub type ExtVal = i32;

#[repr(transparent)]
pub struct Port<'h>(NonNull<()>, PhantomData<&'h ()>);

impl<'h> Port<'h> {
    #[inline(always)]
    pub fn tag(&self) -> Tag {
        let tag = self.0.addr().get() & TAG_MASK;
        assert!(tag != 0);
        unsafe { std::mem::transmute(tag as u8) }
    }

    #[inline(always)]
    pub fn label(&self) -> u16 {
        (self.0.addr().get() >> LABEL_SHIFT) as u16
    }

    pub fn extfn_label(&self) -> ExtFnLabel {
        debug_assert_eq!(self.tag(), Tag::ExtFn);
        self.label().into()
    }

    pub fn op_label(&self) -> OperatorLabel {
        debug_assert_eq!(self.tag(), Tag::Operator);
        self.label().into()
    }

    // SAFETY: `value` must actually be of lifetime `h`
    pub unsafe fn from_parts(tag: Tag, label: u16, value: *mut ()) -> Port<'h> {
        let v = value.map_addr(|x| x | (tag as u8 as usize) | ((label as usize) << LABEL_SHIFT));
        // SAFETY: a valid tag can't be zero, therefore `v` can never be zero
        let p = Port(unsafe { NonNull::new_unchecked(v) }, PhantomData);
        debug_assert_eq!(value, p.addr());
        p
    }

    // SAFETY: `p` must actually be of lifetime `h`
    pub unsafe fn from_raw(p: NonNull<()>) -> Port<'h> {
        Port(p, PhantomData)
    }

    // SAFETY: `p` must actually be of lifetime `h`
    pub unsafe fn option_from_raw(p: *mut ()) -> Option<Port<'h>> {
        if p.addr() & TAG_MASK == 0 {
            None
        } else {
            // SAFETY: Since the tag wasn't zero, the pointer can't be zero
            Some(Self::from_raw(NonNull::new_unchecked(p)))
        }
    }

    pub fn from_extval(e: ExtVal) -> Port<'static> {
        // SAFETY: The value is not a pointer, hence 'static
        unsafe {
            Port::from_parts(
                Tag::ExtVal,
                0,
                std::ptr::null_mut::<()>().map_addr(|_| (e << TAG_SHIFT) as usize),
            )
        }
    }

    pub fn from_global(g: NonNull<Program<'h>>) -> Port<'h> {
        // SAFETY: The value is from a 'h ref, so it's all good
        unsafe {
            Port::from_parts(
                Tag::Global,
                0,
                g.as_ptr() as *mut ()
            )
        }
    }

    pub fn eraser() -> Port<'static> {
        // SAFETY: The value is not a pointer, hence 'static
        unsafe { Port::from_parts(Tag::Eraser, 0, std::ptr::null_mut()) }
    }

    pub fn raw(&self) -> NonNull<()> {
        self.0
    }

    pub fn addr(&self) -> *mut () {
        self.0.as_ptr().map_addr(|x| x & ADDR_MASK)
    }

    pub fn from_wire(w: Wire<'h>) -> Port<'h> {
        let w_ptr = w.to_ptr();
        let v = w_ptr.map_addr(|x| x | (Tag::Wire as u8 as usize));
        let p = Port(v.cast(), PhantomData);
        debug_assert_eq!(w_ptr.as_ptr().cast(), p.addr());
        p
    }

    pub fn to_wire(self) -> Wire<'h> {
        debug_assert_eq!(self.tag(), Tag::Wire);
        // SAFETY: All ports with a wire tag should have been created from a wire
        //         value (i.e. a valid ref), so converting it back should be safe
        Wire::from_ref(unsafe { self.addr().cast::<Word>().as_ref().unwrap() })
    }

    pub fn to_global(self) -> &'h Program<'h> {
        debug_assert_eq!(self.tag(), Tag::Global);
        // SAFETY: Global ports should have been created from a reference to Global
        //         So it's safe to turn it back into one
        unsafe { self.addr().cast::<Program<'h>>().as_ref().unwrap() }
    }

    pub fn to_extval(self) -> ExtVal {
        (self.addr().addr() >> TAG_SHIFT).try_into().unwrap()
    }

    pub fn aux(&self) -> (Wire<'h>, Wire<'h>) {
        debug_assert!(self.tag().is_binary());
        let aux_ptr: *mut Word = self.addr().cast();
        // TODO: Use .is_aligned_to once it is stable
        debug_assert!(aux_ptr.cast::<WordPair>().is_aligned());

        // SAFETY: Binary nodes should always have been created from allocating
        //         Hence their lifetime is bound to `h`, and they should be
        //         `WordPair` aligned and pointing to a valid pair, hence why
        (
            Wire::from_ref(unsafe { aux_ptr.as_ref().unwrap() }),
            Wire::from_ref(unsafe { aux_ptr.offset(1).as_ref().unwrap() }),
        )
    }
}

impl<'h> fmt::Debug for Port<'h> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.tag() {
            Tag::Comb => write!(
                f,
                "C:{:?}:{:08X}",
                CombLabel::from(self.label()),
                self.addr().addr()
            ),
            Tag::Wire => write!(f, "W:{:08X}", self.addr().addr()),
            Tag::ExtVal => write!(f, "EXTVAL:{:08X}", self.addr().addr() >> TAG_SHIFT),
            Tag::Eraser => write!(f, "_"),
            Tag::Global => write!(f, "G:{:08X}", self.addr().addr()),
            Tag::ExtFn => write!(f, "EXTFN:{:?}:{:08X}", self.extfn_label(), self.addr().addr()),
            Tag::Operator => write!(f, "O:{:?}:{:08X}", self.op_label(), self.addr().addr()),
        }
    }
}

impl Clone for Port<'static> {
    fn clone(&self) -> Port<'static> {
        // SAFETY: Making a copy here is safe because any heap type
        //         needs its lifetime bounds to a heap, so any 'static
        //         Port should not contain a pointer and should be safe to blit
        Port(self.0, PhantomData)
    }
}

#[cfg(debug_assertions)]
impl<'h> Drop for Port<'h> {
    fn drop(&mut self) {
        // TODO: Assert for ref-counted ext-vals that they are truely dropped
    }
}
