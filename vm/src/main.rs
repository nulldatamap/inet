mod heap;
mod program;
mod repr;
mod rt;

use std::num::NonZeroUsize;

use heap::*;
use program::*;
use repr::*;
use rt::*;

fn main() {
    let h = Heap::new(NonZeroUsize::new(4096).unwrap());
    let mut rt = Rt::new(&h);
    let prog = unsafe {
        Program {
            reg_count: 2,
            instructions: vec![Inst::Binary(
                Tag::Comb,
                0,
                Reg::ROOT,
                Reg::new(NonZeroUsize::new_unchecked(1)),
                Reg::new(NonZeroUsize::new_unchecked(1)),
            )],
        }
    };
    rt.execute(&prog, Port::from_extval(3));
    println!("{:?}", rt);
}
