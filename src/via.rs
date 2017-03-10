#![allow(dead_code)]

use pack::*;

const ADDR_T1_COUNTER_LO: u16 = 4;
const ADDR_T1_COUNTER_HI: u16 = 5;
const ADDR_T1_LATCH_LO: u16 = 6;
const ADDR_T1_LATCH_HI: u16 = 7;

#[derive(Default)]
pub struct Via {
	ifr_t1: bool,

	t1_counter_lo: u8,
	t1_counter_hi: u8,
	t1_latch_lo: u8,
	t1_latch_hi: u8,
}

impl Via {
	pub fn new() -> Via {
		Default::default()
	}

	pub fn read(&mut self, addr: u16) -> u8 {
		let addr = Self::mask_addr(addr);
		match addr {
			ADDR_T1_COUNTER_LO => {
				self.ifr_t1 = false;
				self.t1_counter_lo
			}
			ADDR_T1_COUNTER_HI => self.t1_counter_hi,
			ADDR_T1_LATCH_LO => self.t1_latch_lo,
			ADDR_T1_LATCH_HI => self.t1_latch_hi,
			_ => {
				error!("Read from unimplemented VIA reg {:01x}", addr);
				0
			}
		}
	}

	pub fn write(&mut self, addr: u16, value: u8) {
		let addr = Self::mask_addr(addr);
		match addr {
			ADDR_T1_COUNTER_LO | ADDR_T1_LATCH_LO => {
				self.t1_latch_lo = value;
			},
			ADDR_T1_LATCH_HI => {
				self.t1_latch_hi = value;
			},
			ADDR_T1_COUNTER_HI => {
				self.t1_latch_hi = value;
				self.t1_counter_hi = self.t1_latch_hi;
				self.t1_counter_lo = self.t1_latch_lo;
				self.ifr_t1 = false;
			}

			_ => warn!("Write to unimplemented VIA reg {:01x} = {:02x}", addr, value)
		}
	}

	fn mask_addr(addr: u16) -> u16 {
		addr & 0xf
	}

	fn t1_counter(&self) -> u16 {
		pack_u16(self.t1_counter_hi, self.t1_counter_lo)
	}

	fn t1_latch(&self) -> u16 {
		pack_u16(self.t1_latch_hi, self.t1_latch_lo)
	}

	fn set_t1_counter(&mut self, value: u16) {
		let (hi, lo) = unpack_u16(value);
		self.t1_counter_hi = hi;
		self.t1_counter_lo = lo;
	}

	fn set_t1_latch(&mut self, value: u16) {
		let (hi, lo) = unpack_u16(value);
		self.t1_latch_hi = hi;
		self.t1_latch_lo = lo;
	}
}