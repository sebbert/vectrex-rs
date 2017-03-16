#![allow(dead_code)]

use pack::*;
use line_sink::*;

const ADDR_T1_COUNTER_LO: u16 = 4;
const ADDR_T1_COUNTER_HI: u16 = 5;
const ADDR_T1_LATCH_LO: u16 = 6;
const ADDR_T1_LATCH_HI: u16 = 7;

#[derive(Default)]
pub struct Via {
	ier_t1: bool,
	ifr_t1: bool,

	acr_t1_continuous: bool,
	acr_t1_pb7: bool,

	t1_counter_lo: u8,
	t1_counter_hi: u8,
	t1_latch_lo: u8,
	t1_latch_hi: u8,
	t1_pb7: bool,
	t1_running: bool,

	orb: u8
}

impl Via {
	pub fn new() -> Via {
		Default::default()	
	}

	pub fn step(&mut self, line_sink: &mut LineSink) -> bool {
		let next_t1_counter = self.t1_counter().wrapping_sub(1);
		self.set_t1_counter(next_t1_counter);
		
		if next_t1_counter == 0xffff {
			self.ifr_t1 = true;
			self.t1_pb7 = !self.t1_pb7;
			let latch = self.t1_latch();
			self.set_t1_counter(latch);

			if !self.acr_t1_continuous {
				self.t1_running = !self.t1_running;
			}
		}

		self.ier() & self.ifr() != 0
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
				self.t1_pb7 = false;
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

	pub fn out_pb7(&self) -> bool {
		if self.acr_t1_pb7 {
			self.t1_pb7
		}
		else {
			self.orb >> 7 == 1
		}
	}

	pub fn ifr(&self) -> u8 {
		(self.ifr_t1 as u8) << 7
	}

	pub fn ier(&self) -> u8 {
		(self.ier_t1 as u8) << 7
	}
}