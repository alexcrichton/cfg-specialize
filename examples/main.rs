use std::str;

use cfg_specialize::cfg_specialize;
#[cfg(target_arch = "x86")]
use std::arch::x86::*;
#[cfg(target_arch = "x86_64")]
use std::arch::x86_64::*;

#[cfg_specialize(target_feature = "ssse3")]
unsafe fn test_ssse3() {
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
unsafe fn add_quickly(a: &[i32], b: &mut [i32]) {
    for (a, b) in a.iter().zip(b) {
        *b += *a;
    }
}

// copied from https://github.com/Matherunner/bin2hex-sse/blob/master/base16_sse4.cpp
#[cfg_specialize(target_feature = "sse4.1")]
unsafe fn hex_encode<'a>(src: &[u8], dst: &'a mut [u8]) -> Result<&'a str, usize> {
    let len = src.len().checked_mul(2).unwrap();
    if dst.len() < len {
        return Err(len);
    }

    if cfg!(target_feature = "sse4.1") {
        hex_encode_sse41(src, dst)
    } else {
        hex_encode_slow(src, dst)
    }
}

#[target_feature(enable = "sse4.1")]
unsafe fn hex_encode_sse41<'a>(mut src: &[u8], dst: &'a mut [u8]) -> Result<&'a str, usize> {
    let ascii_zero = _mm_set1_epi8(b'0' as i8);
    let nines = _mm_set1_epi8(9);
    let ascii_a = _mm_set1_epi8((b'a' - 9 - 1) as i8);
    let and4bits = _mm_set1_epi8(0xf);

    let mut i = 0isize;
    while src.len() >= 16 {
        let invec = _mm_loadu_si128(src.as_ptr() as *const _);

        let masked1 = _mm_and_si128(invec, and4bits);
        let masked2 = _mm_and_si128(_mm_srli_epi64(invec, 4), and4bits);

        // return 0xff corresponding to the elements > 9, or 0x00 otherwise
        let cmpmask1 = _mm_cmpgt_epi8(masked1, nines);
        let cmpmask2 = _mm_cmpgt_epi8(masked2, nines);

        // add '0' or the offset depending on the masks
        let masked1 = _mm_add_epi8(masked1, _mm_blendv_epi8(ascii_zero, ascii_a, cmpmask1));
        let masked2 = _mm_add_epi8(masked2, _mm_blendv_epi8(ascii_zero, ascii_a, cmpmask2));

        // interleave masked1 and masked2 bytes
        let res1 = _mm_unpacklo_epi8(masked2, masked1);
        let res2 = _mm_unpackhi_epi8(masked2, masked1);

        _mm_storeu_si128(dst.as_mut_ptr().offset(i * 2) as *mut _, res1);
        _mm_storeu_si128(dst.as_mut_ptr().offset(i * 2 + 16) as *mut _, res2);
        src = &src[16..];
        i += 16;
    }

    let i = i as usize;
    drop(hex_encode_slow(src, &mut dst[i * 2..]));

    return Ok(str::from_utf8_unchecked(&dst[..src.len() * 2 + i * 2]));
}

fn hex_encode_slow<'a>(src: &[u8], dst: &'a mut [u8]) -> Result<&'a str, usize> {
    for (byte, slots) in src.iter().zip(dst.chunks_mut(2)) {
        slots[0] = hex((*byte >> 4) & 0xf);
        slots[1] = hex((*byte >> 0) & 0xf);
    }

    unsafe { return Ok(str::from_utf8_unchecked(&dst[..src.len() * 2])) }

    fn hex(byte: u8) -> u8 {
        static TABLE: &[u8] = b"0123456789abcdef";
        TABLE[byte as usize]
    }
}

fn main() {
    unsafe {
        test_ssse3();
        add_quickly(&[], &mut []);

        let mut dst = [0; 1024];
        let s = hex_encode(b"This is an extra long message", &mut dst).unwrap();
        println!("{}", s);
    }
}
