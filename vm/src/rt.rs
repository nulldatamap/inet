use core::fmt;

use crate::{
    program::*,
    repr::*,
    heap::*,
};

macro_rules! sym {
    ($p:pat) => { ($p, $p) };
    ($a:pat, $b:pat) => {($a, $b) | ($b, $a)};
    ($p:pat, $a:ident, $b:ident) => { (($p, $a), ($p, $b)) };
}

pub struct Rt<'h> {
    pub(crate) allocator: Allocator<'h>,

    active_fast: Vec<(Port<'h>, Port<'h>)>,
    active_slow: Vec<(Port<'h>, Port<'h>)>,

    registers: Vec<Option<Port<'h>>>,
}

impl<'h> Rt<'h> {
    pub fn new(heap: &'h Heap) -> Rt<'h> {
        Rt {
            allocator: Allocator::new(heap),

            active_fast: Vec::new(),
            active_slow: Vec::new(),

            registers: Vec::new(),
        }
    }

    fn dup(&mut self, p: &Port<'h>) -> Port<'h> {
        // TODO: Proper dup'ing
        unsafe { Port::option_from_raw(p.raw().as_ptr()).unwrap() }
    }

    // TODO: Proper erasing
    fn erase(&mut self, p: Port<'h>) {}

    fn follow_wire(&mut self, mut p: Port<'h>) -> Port<'h> {
        while p.tag() == Tag::Wire {
            let w = p.to_wire();
            if let Some(x) = w.load() {
                self.allocator.free_wire(w);
                p = x;
            } else {
                return Port::from_wire(w)
            }
        }
        unreachable!()
    }

    fn link_wire(&mut self, w: Wire<'h>, p: Port<'h>) {
        let p = self.follow_wire(p);
        if let Some(old_x) = w.swap(Some(self.dup(&p))) {
            self.allocator.free_wire(w);
            self.link(old_x, p);
        }
    }

    fn link(&mut self, a: Port<'h>, b: Port<'h>) {
        match ((a.tag(), a), (b.tag(), b)) {
            sym!((Tag::Wire, a), (_, b)) => self.link_wire(a.to_wire(), b),
            sym!(Tag::Eraser | Tag::Global, a, b)
                | sym!(Tag::Eraser | Tag::ExtVal, a, b) => {
                    self.erase(a);
                    self.erase(b);
                },
            ((_, a), (_, b)) => {
                // TODO: Split into both slow and fast
                self.active_fast.push((a, b));
            }
        }
    }


    fn interact(&mut self, a: Port<'h>, b: Port<'h>) {
    }

    fn link_register(&mut self, r : Reg, p: Port<'h>) {
        if let Some(v) = self.registers[r.index()].take() {
            self.link(p, v);
        } else {
            self.registers[r.index()] = Some(p);
        }
    }

    pub fn execute(&mut self, prog: &Program, p: Port<'h>) {
        self.registers.resize_with(prog.reg_count, || None);

        self.link_register(Reg::ROOT, p);

        for inst in &prog.instructions {
            match inst {
                &Inst::Nilary(r, ref v) => {
                    let v = self.dup(v);
                    self.link_register(r, v);
                },
                &Inst::Binary(tag, lbl, a, l, r) => {
                    let (p, lw, rw) = self.allocator.alloc_node(tag, lbl);
                    self.link_register(a, p);
                    self.link_register(l, Port::from_wire(lw));
                    self.link_register(r, Port::from_wire(rw));
                }
            }
        }

        debug_assert!(self.registers.iter().all(|r| r.is_none()));
    }
}


impl<'h> fmt::Debug for Rt<'h> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for w in self.allocator.active_area().iter() {
            let v = unsafe { Port::option_from_raw(w.load()) };
            write!(f, "{:08X}: ", w as *const Word as usize)?;
            if let Some(p) = v {
            write!(f, "{:?}\n", p)?;

            } else {
                write!(f, "-\n")?;
            }
        }

        for (a, b) in self.active_fast.iter() {
            write!(f, "- {:?} <> {:?}\n", a, b)?;
        }

        for (a, b) in self.active_slow.iter() {
            write!(f, "= {:?} <> {:?}\n", a, b)?;
        }

        Ok(())
    }
}
