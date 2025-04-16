use std::num::NonZeroUsize;

use crate::repr::{Port, Tag};

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Reg(usize);

impl Reg {
    pub const ROOT: Reg = Reg(0);

    pub const fn new(r: NonZeroUsize) -> Reg { Reg(r.get()) }

    pub fn index(&self) -> usize { self.0 }
}

pub enum Inst {
    Nilary(Reg, Port<'static>),
    Binary(Tag, u16, Reg, Reg, Reg),
}

pub struct Program {
    pub reg_count: usize,
    pub instructions: Vec<Inst>,
}
