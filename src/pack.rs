#![allow(dead_code)]

#[inline]
pub fn pack_u16(hi: u8, lo: u8) -> u16 {
	let hi = hi as u16;
	let lo = lo as u16;

	(hi << 8) | lo
}

#[inline]
pub fn unpack_u16(value: u16) -> (u8, u8) {
	let hi = (value >> 8) as u8;
	let lo = value as u8;

	(hi, lo)
}

#[inline]
pub fn pack_nibbles(hi: u8, lo: u8) -> u8 {
	(hi << 4) | (lo & 0x0f)
}

#[inline]
pub fn unpack_nibbles(value: u8) -> (u8, u8) {
	let hi = value >> 4;
	let lo = value & 0x0f;

	(hi, lo)
}

#[inline]
pub fn pack_flags(flags: [bool;8]) -> u8 {
	(flags[0] as u8) << 0 |
	(flags[1] as u8) << 1 |
	(flags[2] as u8) << 2 |
	(flags[3] as u8) << 3 |
	(flags[4] as u8) << 4 |
	(flags[5] as u8) << 5 |
	(flags[6] as u8) << 6 |
	(flags[7] as u8) << 7
}

#[inline]
pub fn unpack_flag(value: u8, bit_index: u8) -> bool {
	1 == (value >> (bit_index & 0x7)) & 1
}