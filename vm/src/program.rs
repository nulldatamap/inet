use core::fmt;
use std::{cell::UnsafeCell, collections::HashMap, marker::PhantomPinned, num::NonZeroUsize, ptr::NonNull};

use crate::repr::{CombLabel, ExtFnLabel, OperatorLabel, Port, Tag};

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Reg(usize);

impl Reg {
    pub const ROOT: Reg = Reg(0);

    pub const fn new(r: NonZeroUsize) -> Reg {
        Reg(r.get())
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

impl fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

pub enum UnlinkedInst {
    Nilary(Reg, Port<'static>),
    Binary(Tag, u16, Reg, Reg, Reg),
    Global(Reg, usize),
}

impl fmt::Debug for UnlinkedInst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &UnlinkedInst::Nilary(r, ref p) => write!(f, "/{:?} {:?}", p, r),
            &UnlinkedInst::Binary(t, lbl, a, l, r) => {
                write!(f, "/{:?}:", t)?;
                match t {
                    Tag::Comb => write!(f, "{:?} ", CombLabel::from(lbl)),
                    Tag::Operator => write!(f, "{:?} ", OperatorLabel::from(lbl)),
                    Tag::ExtFn => write!(f, "{:?} ", ExtFnLabel::from(lbl)),
                    _ => panic!("Invlaid tag `{:?}` for binary instruction", t),
                }?;
                write!(f, " {:?} {:?} {:?}", a, l, r)
            }
            &UnlinkedInst::Global(r, i) => write!(f, "G:{} {:?}", i, r),
        }
    }
}

pub struct UnlinkedProgram {
    pub name: String,
    pub globals: Vec<String>,
    pub reg_count: usize,
    pub instructions: Vec<UnlinkedInst>,
}

impl fmt::Debug for UnlinkedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "program {}[{}]:\n", self.name, self.reg_count)?;
        for i in &self.instructions {
            if let UnlinkedInst::Global(r, i) = i {
                write!(f, "  G:{} {:?}\n", self.globals[*i], r)?;
            } else {
                write!(f, "  {:?}\n", i)?;
            }
        }
        Ok(())
    }
}

pub enum Inst<'h> {
    Nilary(Reg, Port<'h>),
    Binary(Tag, u16, Reg, Reg, Reg),
}

pub struct Program<'h> {
    pub reg_count: usize,
    pub instructions: Vec<Inst<'h>>,
    // TODO: Use UnsafePinned once stabilized
    _pinned: PhantomPinned,
}

pub struct Globals<'h> {
    storage: std::pin::Pin<Box<[UnsafeCell<Program<'h>>]>>,
    name_to_global: HashMap<String, NonNull<Program<'h>>>,
}

impl<'h> Globals<'h> {
    pub fn get(&self, n: &str) -> Option<NonNull<Program<'h>>> {
        self.name_to_global.get(n).copied()
    }
}

pub type LinkError = String;

pub fn link_programs<'h>(progs: &[UnlinkedProgram]) -> Result<Globals<'h>, LinkError> {
    // Reserve and pin all the globals up from, so we can create
    // references between programs
    let gs = progs
        .iter()
        .map(|p| {
            UnsafeCell::new(Program {
                reg_count: p.reg_count,
                instructions: Vec::new(),
                _pinned: PhantomPinned,
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let gs = Box::into_pin(gs);
    // Build the name -> global mapping
    let mut name_to_global = HashMap::new();
    for (i, prog) in progs.iter().enumerate() {
        // Since the backing memory is pinned any pointers we create to the
        // programs are going to not chance address:
        let g = gs[i].get() as *const Program<'h>;
        // SAFETY: We going from UnsafeCell<Program<'h>> to juset Program<'h>
        //         So we're effectively "freezing" the program, which will happen
        //         For the entire struct later. This is only safe to do at this point
        //         Because this pointer will only be actually dereferenced after
        //         The rest of the structure has been frozen
        let frozen_g = unsafe { std::mem::transmute::<_, NonNull<Program<'h>>>(g) };
        name_to_global.insert(prog.name.clone(), frozen_g);
    }

    for (i, prog) in progs.iter().enumerate() {
        let g = gs.as_ref()[i].get();
        let insts = prog
            .instructions
            .iter()
            .map(|inst| -> Result<_, LinkError> {
                Ok(match inst {
                    &UnlinkedInst::Nilary(r, ref p) => Inst::Nilary(r, p.clone()),
                    &UnlinkedInst::Binary(tag, lbl, a, l, r) => Inst::Binary(tag, lbl, a, l, r),
                    &UnlinkedInst::Global(r, g) => {
                        let name = &prog.globals[g];
                        let g_ref = name_to_global
                            .get(name)
                            .ok_or_else(|| format!("Linking failed: undefined name `{}`", g))?;
                        // SAFETY: The NonNull<Program<'h>> came from a &'h Program<'h>
                        //         So it's safe to turn it back into one
                        Inst::Nilary(r, Port::from_global(*g_ref))
                    }
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        // SAFETY: There can't be any observers to this internal mutation
        //         And it's the only one performed before the entire structure is freezed
        (unsafe { &mut *g }).instructions.extend(insts.into_iter());
    }

    // SAFETY: We're getting rid of the UnsafeCell, i.e. freezen the struct
    //         now that we're doing setting up the internal cyclic refs
    //         All the pointers to the programs inside are now also "safe" to use
    Ok(Globals {
        storage: gs,
        name_to_global,
    })
}
