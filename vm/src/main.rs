mod compiler;
mod ext;
mod heap;
mod program;
mod repr;
mod rt;

use std::num::NonZeroUsize;

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
                    print(print(print(print(v("io"), v("x")), v("y")), v("z")), v("w")),
                ),
            ),
        ),
        def("three", lam(vec![], i(3))),
        def("id", lam(vec!["x"], v("x"))),
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
