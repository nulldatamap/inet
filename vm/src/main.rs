mod compiler;
mod heap;
mod program;
mod repr;
mod rt;

use std::num::NonZeroUsize;

use compiler::*;
use heap::*;
use program::*;
use repr::*;
use rt::*;

fn main() {
    let h = Heap::new(NonZeroUsize::new(4096).unwrap());
    let mut rt = Rt::new(&h);
    let uprog = Compiler::compile(vec![
        def("main", letv("x", v("c"), letv("y", v("x"), v("x")))),
        def("c", i(3)),
    ])
    .unwrap();
    println!("{:?}", uprog);
    let globals = link_programs(&uprog).unwrap();

    rt.link(
        Port::from_global(globals.get("main").unwrap()),
        Port::from_extval(3),
    );

    println!("{:?}", rt);

    loop {
        while let Some((a, b)) = rt.active_fast.pop() {
            rt.interact(a, b);
            println!("{:?}", rt);
        }

        if let Some((a, b)) = rt.active_slow.pop() {
            rt.interact(a, b);
            println!("{:?}", rt);
        } else {
            break;
        }
    }
}
