use crate::{repr::ExtVal, rt::Rt};


type ExtFnF = for<'h> fn(&mut Rt<'h>, ExtVal, ExtVal) -> ExtVal;

pub struct Externals {
    pub extfns: Vec<ExtFnF>,
}

pub fn builtins() -> Externals {
    Externals {
        extfns: vec![
            |_rt, l, r| {
                println!("{:?}", r);
                l
            },
        ]
    }
}
