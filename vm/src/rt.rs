use core::fmt;
use std::any::TypeId;

use crate::{
    ext::{ExtVal, Externals, UniqueCell},
    heap::*,
    program::*,
    repr::*,
};

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
        // SAFETY: It's safe to blit `p` here because:
        //         - If the wire we put it into was empty, then we
        //           forgot about the other copy, and the effect was non-copying
        //         - If the wire wasn't empty, then it gets overwritten (forgetting the blitted `p`)
        //           And then `p` is just used
        if let Some(old_x) = w.swap(Some(unsafe { p.blit() })) {
            self.allocator.free_wire(w);
            self.link(old_x, p);
        } else {
            // We forget `p` here because we stored a blitted copy in the wire
            // Since it's going to stay there, we want to forget this copy
            // so that it doesn't get dropped at the end of this scope
            std::mem::forget(p);
        }
    }

    pub fn link(&mut self, a: Port<'h>, b: Port<'h>) {
        match ((a.tag(), a), (b.tag(), b)) {
            sym!((Tag::Wire, a), (_, b)) => self.link_wire(a.to_wire(), b),
            sym!(Tag::Eraser | Tag::Global, a, b) | sym!(Tag::Eraser | Tag::ExtVal, a, b) => {
                a.erase(&self.externals);
                b.erase(&self.externals)
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
        self.link_wire(l, a.dup());
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

    fn branch(&mut self, br: Port<'h>, c: Port<'h>) {
        let (br_tf, res) = br.aux();
        let (sel, p, q) = self
            .allocator
            .alloc_node(Tag::Operator, OperatorLabel::Branch.into());
        let (t, f) = if c.to_extval().is_truthy() {
            (p, q)
        } else {
            (q, p)
        };
        self.link_wire(t, Port::from_wire(res));
        self.link_wire(f, Port::eraser());
        self.link_wire(br_tf, sel);
    }

    fn call(&mut self, f: Port<'h>, x: Port<'h>) {
        let (y, res) = f.aux();
        if let Some(y_val) = y.load() {
            if y_val.tag() == Tag::ExtVal {
                self.allocator.free_wire(y);
                let (flipped, idx) = f.extfn_label().get();
                let (a, b) = if flipped { (y_val, x) } else { (x, y_val) };
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
                    let v = v.dup();
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

    pub fn get_cell<'a, T: Sync + 'static>(&self, v: &'a ExtVal<'h>) -> &'a T {
        debug_assert!(self.externals.is_type::<T>(v.ty()));
        // SAFETY: We've validated the TypeId of T
        unsafe { v.get_ref_unchecked() }
    }

    pub fn get_unique<T: Sync + 'static>(&self, v: ExtVal<'h>) -> UniqueCell<T> {
        debug_assert!(self.externals.is_type::<T>(v.ty()));
        // SAFETY: We've validated the TypeId of T
        unsafe { v.get_unique_unchecked() }
    }

    pub fn erase(&self, p: ExtVal<'h>) {
        p.erase(&self.externals);
    }
}

impl<'h> fmt::Debug for Rt<'h> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for w in self.allocator.active_area().iter() {
            let v = unsafe { Port::option_from_raw(w.load()) };
            write!(f, "{:08X}: ", w as *const Word as usize)?;
            if let Some(p) = v {
                write!(f, "{:?}\n", &p)?;
                std::mem::forget(p);
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
