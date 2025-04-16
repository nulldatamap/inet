use core::fmt;
use std::sync::atomic::{AtomicPtr, Ordering::Relaxed};
use std::{marker::PhantomData, ptr::NonNull};

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
const ADDR_MASK: usize = !(TAG_MASK | 0xFFusize << LABEL_SHIFT);

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Tag {
    ExtVal = 1,
    ExtFn = 2,
    Global = 3,
    Comb = 4,
    Operator = 5,
    Wire = 6,
    Eraser = 7,
}

impl Tag {
    fn is_binary(self) -> bool {
        matches!(self, Tag::Comb | Tag::Operator)
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

    // SAFETY: `value` must actually be of lifetime `h`
    pub unsafe fn from_parts(tag: Tag, label: u16, value: *mut ()) -> Port<'h> {
        let v = value.map_addr(|x| x | (tag as u8 as usize) | ((label as usize) << LABEL_SHIFT));
        // SAFETY: a valid tag can't be zero, therefore `v` can never be zero
        let p = Port(unsafe { NonNull::new_unchecked(v) }, PhantomData);
        debug_assert_eq!(value, p.addr());
        p
    }

    // SAFETY: `p` must actually be of lifetime `h`
    pub unsafe fn option_from_raw(p: *mut ()) -> Option<Port<'h>> {
        if p.addr() & TAG_MASK == 0 {
            None
        } else {
            // SAFETY: Since the tag wasn't zero, the pointer can't be zero
            Some(Port(NonNull::new_unchecked(p.cast()), PhantomData))
        }
    }

    pub fn from_extval(e: i32) -> Port<'static> {
        // SAFETY: The value is not a pointer, hence 'static
        unsafe {
            Port::from_parts(
                Tag::ExtVal,
                0,
                std::ptr::null_mut::<()>().map_addr(|_| (e << TAG_SHIFT) as usize),
            )
        }
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
            Tag::Comb => write!(f, "C:{:04X}:{:08X}", self.label(), self.addr().addr()),
            Tag::Wire => write!(f, "W:{:08X}", self.addr().addr()),
            Tag::ExtVal => write!(f, "E:{:08X}", self.addr().addr() >> TAG_SHIFT),
            Tag::ExtFn => todo!(),
            Tag::Global => todo!(),
            Tag::Operator => todo!(),
            Tag::Eraser => todo!(),
        }
    }
}

#[cfg(debug_assertions)]
impl<'h> Drop for Port<'h> {
    fn drop(&mut self) {
        // TODO: Assert for ref-counted ext-vals that they are truely dropped
    }
}
