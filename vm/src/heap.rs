use std::{
    alloc::{alloc_zeroed, Layout},
    mem,
    num::NonZeroUsize,
    ptr,
};

use crate::repr::*;

#[repr(transparent)]
pub struct Heap(pub(crate) [WordPair]);

impl Heap {
    pub fn new(n: NonZeroUsize) -> Box<Self> {
        unsafe {
            // SAFETY: The layout can't be zero-sized
            let p: *mut WordPair = alloc_zeroed(Layout::array::<WordPair>(n.get()).unwrap()).cast();
            if p.is_null() {
                panic!("Failed to allocate heap!");
            }
            // SAFETY: The slice is safe to construct due to the allocted layout matching
            //         And the transmute is safe because of the #[repr(transparent)] on Heap
            Box::from_raw(mem::transmute(ptr::slice_from_raw_parts_mut(p, n.get())))
        }
    }

    fn slice(&self) -> &[WordPair] {
        &self.0[..]
    }
}

pub struct Allocator<'h> {
    heap: &'h Heap,
    next: usize,
}

impl<'h> Allocator<'h> {
    pub fn new(heap: &'h Heap) -> Allocator<'h> {
        Allocator { next: 0, heap }
    }

    pub(crate) fn active_area(&self) -> &[Word] {
        unsafe { &std::mem::transmute::<_, &[Word]>(self.heap.slice())[..self.next * 2] }
    }

    pub fn free_wire(&mut self, w: Wire<'h>) {
        // TODO: Free list
        w.store(None);
    }

    pub fn alloc_node(&mut self, tag: Tag, label: u16) -> (Port<'h>, Wire<'h>, Wire<'h>) {
        let h = self.heap.slice();
        if self.next >= h.len() {
            panic!("Out of heap memory");
        }

        let p = &h[self.next];
        self.next += 1;
        p.0.store(std::ptr::null_mut());
        p.1.store(std::ptr::null_mut());
        let pp: *const WordPair = p;
        // SAFETY: `pp`'s lifetime is bound to `h`
        let port = unsafe { Port::from_parts(tag, label, pp.cast::<()>().cast_mut()) };
        (port, Wire::from_ref(&p.0), Wire::from_ref(&p.1))
    }
}
