extern crate cupid;

macro_rules! checker {
    ($(($function:ident, $method:ident),)*) => ($(
        pub fn $function() -> bool {
            cupid::master().unwrap().$method()
        }
    )*)
}

checker! {
    (check_avx, avx),
    (check_ssse3, ssse3),
    (check_sse2, sse2),
    (check_sse41, sse4_1),
}
