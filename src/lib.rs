#![feature(use_extern_macros)]

extern crate cfg_specialize_macros; // the warning here is a lie

pub use cfg_specialize_macros::*;

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
pub use self::x86::*;
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
mod x86;
