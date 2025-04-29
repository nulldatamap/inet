#![feature(
    thin_box,
    ptr_metadata,
    ptr_as_ref_unchecked,
    non_null_from_ref,
    unsize,
    layout_for_ptr
)]
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
                letv(
                    "x",
                    extcall(Externals::MK_ARR, vec![i(3), i(0)]),
                    print(
                        v("io"),
                        extcall(
                            Externals::ARR_GET,
                            vec![extcall(Externals::ARR_SET, vec![v("x"), i(1), i(99)]), i(1)],
                        ),
                    ),
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
    rt.erase_unique(io);
}
