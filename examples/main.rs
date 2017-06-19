#![feature(proc_macro, cfg_target_feature, target_feature)]

extern crate cfg_specialize;
extern crate stdsimd;

use cfg_specialize::cfg_specialize;

#[cfg_specialize(target_feature = "ssse3")]
fn test_ssse3() {
    if cfg!(target_feature = "ssse3") {
        // This code runs if the cpu it's running on has `ssse3`, and `ssse3`
        // features are also enabled here.
        println!("I've got ssse3");
    } else {
        // This code executes if the cpu does not have `ssse3` enabled.
        println!("no ssse3");
    }
}

// Two versions of this function are generated, one for avx and one without.
// You'll notice that 256 bit simd is used for one version and 128 for the
// other. This function will execute the avx version for cpus that support avx.
#[cfg_specialize(target_feature = "avx")]
fn add_quickly(a: &[i32], b: &mut [i32]) {
    for (a, b) in a.iter().zip(b) {
        *b += *a;
    }
}

fn main() {
    test_ssse3();
    add_quickly(&[], &mut []);
}
