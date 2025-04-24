#![feature(thin_box, ptr_metadata, ptr_as_ref_unchecked, non_null_from_ref)]
mod compiler;
mod ext;
mod heap;
mod program;
mod repr;
mod rt;

use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicUsize, Ordering};

use compiler::*;
use ext::*;
use heap::*;
use program::*;
use repr::*;
use rt::*;

fn main() {
    let h = Heap::new(NonZeroUsize::new(4096).unwrap());
    let mut rt = Rt::new(&h, Externals::builtins());
    let uprog = Compiler::compile(vec![
        def(
            "main",
            lam(
                vec!["io"],
                untup(
                    vec!["x", "y", "z", "w"],
                    tup(vec![i(3), i(4), i(5), i(6)]),
                    print(print(print(print(v("io"), v("x")), v("y")), v("z")), call(v("id"), vec![v("w")])),
                ),
            ),
        ),
        def("three", lam(vec![], i(3))),
        def("id", lam(vec!["x"], v("x"))),
    ])
    .unwrap();
    println!("{:?}", uprog);
    let globals = link_programs(&uprog).unwrap();
    let io = ExtVal::cell(
        Externals::IO_TY,
        IoHandle {
            op_count: AtomicUsize::new(0),
        },
    );

    rt.link(
        Port::from_global(globals.get("main").unwrap()),
        Port::from_extval(io.dup()),
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

    let io = rt.get_unique::<IoHandle>(io);
    println!(
        "IO actions performed: {}\n",
        io.op_count.load(Ordering::Relaxed)
    );
}
