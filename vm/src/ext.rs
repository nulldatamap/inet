use crate::{repr::ExtVal, rt::Rt};


type ExtFnF = for<'h> fn(&mut Rt<'h>, ExtVal, ExtVal) -> ExtVal;


pub struct Externals {
    pub extfns: Vec<ExtFnF>,
}

impl Externals {
    pub const SEQ: u16 = 0;
    pub const ADD: u16 = 1;
    pub const PRINT: u16 = 2;

    pub fn builtins() -> Externals {
        Externals {
            extfns: vec![
                |_rt, l, _r| {
                    l
                },
                |_rt, l, r| {
                    l + r
                },
                |_rt, l, r| {
                    println!("{:?}", r);
                    l
                },
            ]
        }
    }
}
