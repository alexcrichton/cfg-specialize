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
}
