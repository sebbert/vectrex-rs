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