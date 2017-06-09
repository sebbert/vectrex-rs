#![allow(dead_code)]

use std::ops::*;

#[inline]
pub fn pack_16(hi: u8, lo: u8) -> u16 {
	let hi = hi as u16;
	let lo = lo as u16;

	(hi << 8) | lo
}

#[inline]
pub fn unpack_16(value: u16) -> (u8, u8) {
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

pub fn unpack_flags(value: u8) -> [bool;8] {
	[
		value.get_flag(0),
		value.get_flag(1),
		value.get_flag(2),
		value.get_flag(3),
		value.get_flag(4),
		value.get_flag(5),
		value.get_flag(6),
		value.get_flag(7)
	]
}

pub struct Flag<T:Flags> {
	value: T,
	flag_index: usize
}

impl<T:Flags> Flag<T> {
	pub fn set(self, value: bool) -> T {
		self.value.set_flag(self.flag_index, value)
	}
	
	pub fn set_and(mut self, flag_value: bool) -> Self {
		self.value = self.value.set_flag(self.flag_index, flag_value);
		self
	}
	
	pub fn get(self) -> bool {
		self.value.get_flag(self.flag_index)
	}
}

pub trait Flags : Sized {
	fn get_flag(self, flag_index: usize) -> bool;
	fn set_flag(self, flag_index: usize, flag_value: bool) -> Self;
	fn flag(self, flag_index: usize) -> Flag<Self>;
}

impl Flags for u8 {
	fn get_flag(self, flag_index: usize) -> bool {
		1 == (self >> flag_index) & 1
	}
	
	fn set_flag(self, flag_index: usize, flag_value: bool) -> Self {
		self & ((!1) << flag_index) | ((flag_value as u8) << flag_index)
	}
	
	fn flag(self, flag_index: usize) -> Flag<Self> {
		Flag {
			value: self,
			flag_index: flag_index
		}
	}
}

impl Flags for u16 {
	fn get_flag(self, flag_index: usize) -> bool {
		1 == (self >> flag_index) & 1
	}
	
	fn set_flag(self, flag_index: usize, flag_value: bool) -> Self {
		self & ((!1) << flag_index) | ((flag_value as u16) << flag_index)
	}
	
	fn flag(self, flag_index: usize) -> Flag<Self> {
		Flag {
			value: self,
			flag_index: flag_index
		}
	}
}

impl Flags for u32 {
	fn get_flag(self, flag_index: usize) -> bool {
		1 == (self >> flag_index) & 1
	}
	
	fn set_flag(self, flag_index: usize, flag_value: bool) -> Self {
		self & ((!1) << flag_index) | ((flag_value as u32) << flag_index)
	}
	
	fn flag(self, flag_index: usize) -> Flag<Self> {
		Flag {
			value: self,
			flag_index: flag_index
		}
	}
}