use core::slice;

pub const fn rounded_div(n: u32, d: u32) -> u32 {
    let floored_div = n / d;
    let rem = n % d;
    let mut half_d = d / 2;
    if d % 2 != 0 {
        half_d += 1;
    }
    if rem >= half_d {
        floored_div + 1
    } else {
        floored_div
    }
}

pub fn u16_slice_as_u8(word: &[u16]) -> &[u8] {
    let new_len = word.len() * 2;
    let new_ptr = word.as_ptr() as *const u8;
    // Safety: A 16bit int can be read as two bytes
    unsafe { slice::from_raw_parts(new_ptr, new_len) }
}

const fn min(x: u32, y: u32) -> u32 {
    if x < y {
        x
    } else {
        y
    }
}

pub const fn html_color_to_gba(html: u32) -> u16 {
    let red = min(rounded_div((html >> 16) & 0xFF, 8), 31);
    let green = min(rounded_div((html >> 8) & 0xFF, 8), 31);
    let blue = min(rounded_div(html & 0xFF, 8), 31);
    (red | (green << 5) | (blue << 10)) as u16
}
