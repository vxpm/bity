#![allow(non_camel_case_types)]
mod helper {
    pub type U8 = u8;
    pub type U16 = u16;
    pub type U32 = u32;
    pub type U64 = u64;
}

pub use arbitrary_int::*;
pub type u8 = helper::U8;
pub type u16 = helper::U16;
pub type u32 = helper::U32;
pub type u64 = helper::U64;
