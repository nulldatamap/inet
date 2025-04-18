use core::fmt;

use crate::{ext::Externals, heap::*, program::*, repr::*};

macro_rules! sym {
    ($p:pat) => {
        ($p, $p)
    };
    ($a:pat, $b:pat) => {
        ($a, $b) | ($b, $a)
    };
    ($p:pat, $a:ident, $b:ident) => {
        (($p, $a), ($p, $b))
    };
}

pub struct Rt<'h> {
    pub(crate) allocator: Allocator<'h>,
    pub(crate) externals: Externals,

    pub(crate) active_fast: Vec<(Port<'h>, Port<'h>)>,
    pub(crate) active_slow: Vec<(Port<'h>, Port<'h>)>,

    registers: Vec<Option<Port<'h>>>,
}

impl<'h> Rt<'h> {
    pub fn new(heap: &'h Heap, exts: Externals) -> Rt<'h> {
        Rt {
            allocator: Allocator::new(heap),
            externals: exts,

            active_fast: Vec::new(),
            active_slow: Vec::new(),

            registers: Vec::new(),
        }
    }

    pub fn dup(&mut self, p: &Port<'h>) -> Port<'h> {
        // TODO: Proper dup'ing
        unsafe { Port::option_from_raw(p.raw().as_ptr()).unwrap() }
    }

    // TODO: Proper erasing
    pub fn erase(&mut self, p: Port<'h>) {}

    fn follow_wire(&mut self, mut p: Port<'h>) -> Port<'h> {
        while p.tag() == Tag::Wire {
            let w = p.to_wire();
            if let Some(x) = w.load() {
                self.allocator.free_wire(w);
                p = x;
            } else {
                return Port::from_wire(w);
            }
        }
        p
    }

    fn link_wire(&mut self, w: Wire<'h>, p: Port<'h>) {
        let p = self.follow_wire(p);
        if let Some(old_x) = w.swap(Some(self.dup(&p))) {
            self.allocator.free_wire(w);
            self.link(old_x, p);
        }
    }

    pub fn link(&mut self, a: Port<'h>, b: Port<'h>) {
        match ((a.tag(), a), (b.tag(), b)) {
            sym!((Tag::Wire, a), (_, b)) => self.link_wire(a.to_wire(), b),
            sym!(Tag::Eraser | Tag::Global, a, b) | sym!(Tag::Eraser | Tag::ExtVal, a, b) => {
                self.erase(a);
                self.erase(b);
            }
            ((_, a), (_, b)) => {
                // TODO: Split into both slow and fast
                self.active_fast.push((a, b));
            }
        }
    }

    pub(crate) fn interact(&mut self, a: Port<'h>, b: Port<'h>) {
        use Tag::*;

        match ((a.tag(), a), (b.tag(), b)) {
            sym!((Wire, _), _) | sym!((Eraser | ExtVal, _)) => unreachable!(),
            // TODO: sym!(((Global, a), (Comb, b))) if labels bla bla
            sym!((Global, a), (_, b)) => self.expand(a, b),
            sym!((Eraser, a), (_, b)) | sym!((ExtVal, a), (Comb, b)) => self.copy(a, b),
            ((Comb, a), (Comb, b)) | ((ExtFn, a), (ExtFn, b)) | ((Operator, a), (Operator, b))
                if a.label() == b.label() =>
            {
                self.annihilate(a, b)
            }
            ((Comb | ExtFn | Operator, a), (Comb | ExtFn | Operator, b)) => self.commute(a, b),
            sym!((Operator, a), (ExtVal, b)) if a.label() == OperatorLabel::Branch as u16 => {
                self.branch(a, b)
            }
            sym!((ExtFn, a), (ExtVal, b)) => self.call(a, b),
            sym!((Operator, a), (_, b)) => {
                panic!("Unimplemented operator interaction {:?} <> {:?}", a, b)
            }
        }
    }

    fn copy(&mut self, a: Port<'h>, b: Port<'h>) {
        let (l, r) = b.aux();
        let a0 = self.dup(&a);
        self.link_wire(l, a0);
        self.link_wire(r, a);
    }

    fn annihilate(&mut self, a: Port<'h>, b: Port<'h>) {
        let (al, ar) = a.aux();
        let (bl, br) = b.aux();
        self.link_wire(al, Port::from_wire(bl));
        self.link_wire(ar, Port::from_wire(br));
    }

    fn commute(&mut self, a: Port<'h>, b: Port<'h>) {
        let (a0, a0l, a0r) = self.allocator.alloc_node(a.tag(), a.label());
        let (a1, a1l, a1r) = self.allocator.alloc_node(a.tag(), a.label());
        let (b0, b0l, b0r) = self.allocator.alloc_node(b.tag(), b.label());
        let (b1, b1l, b1r) = self.allocator.alloc_node(b.tag(), b.label());

        self.link_wire(a0l, Port::from_wire(b0l));
        self.link_wire(a0r, Port::from_wire(b1l));
        self.link_wire(a1l, Port::from_wire(b0r));
        self.link_wire(a1r, Port::from_wire(b1r));

        let (al, ar) = a.aux();
        let (bl, br) = b.aux();

        self.link_wire(al, b0);
        self.link_wire(ar, b1);
        self.link_wire(bl, a0);
        self.link_wire(br, a1);
    }

    fn expand(&mut self, g: Port<'h>, p: Port<'h>) {
        let prog = g.to_global();
        self.execute(prog, p);
    }

    fn branch(&mut self, a: Port<'h>, b: Port<'h>) {
        todo!()
    }

    fn call(&mut self, f: Port<'h>, x: Port<'h>) {
        let (y, res) = f.aux();
        if let Some(y_val) = y.load() {
            if y_val.tag() == Tag::ExtVal {
                self.allocator.free_wire(y);
                let (flipped, idx) = f.extfn_label().get();
                let (a, b) = if flipped { (x, y_val) } else { (y_val, x) };
                let r = self.externals.extfns[idx](self, a.to_extval(), b.to_extval());
                self.link_wire(res, Port::from_extval(r));
                return;
            }
        }

        let (new_f, yw, resw) = self
            .allocator
            .alloc_node(Tag::ExtFn, f.extfn_label().flip().into());
        self.link_wire(y, new_f);
        self.link_wire(resw, Port::from_wire(res));
        self.link_wire(yw, x);
    }

    fn link_register(&mut self, r: Reg, p: Port<'h>) {
        if let Some(v) = self.registers[r.index()].take() {
            self.link(p, v);
        } else {
            self.registers[r.index()] = Some(p);
        }
    }

    pub fn execute(&mut self, prog: &'h Program<'h>, p: Port<'h>) {
        self.registers.resize_with(prog.reg_count, || None);

        self.link_register(Reg::ROOT, p);

        for inst in &prog.instructions {
            match inst {
                &Inst::Nilary(r, ref v) => {
                    let v = self.dup(v);
                    self.link_register(r, v);
                }
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
