#![allow(dead_code)]

use pack::*;
use line_sink::*;

const REG_IO_B: u16 = 0x0;
const REG_IO_A: u16 = 0x1;
const REG_DDRB: u16 = 0x2;
const REG_DDRA: u16 = 0x3;
const REG_T1_COUNTER_LO: u16 = 0x4;
const REG_T1_COUNTER_HI: u16 = 0x5;
const REG_T1_LATCH_LO: u16 = 0x6;
const REG_T1_LATCH_HI: u16 = 0x7;
const REG_T2_LO: u16 = 0x8;
const REG_T2_HI: u16 = 0x9;
const REG_SHIFT: u16 = 0xa;
const REG_ACR: u16 = 0xb;
const REG_PCR: u16 = 0xc;
const REG_IFR: u16 = 0xd;
const REG_IER: u16 = 0xe;
const REG_IO_A_NO_HANDSHAKE: u16 = 0xf;

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

	pub fn step(&mut self, _: &mut LineSink) -> bool {
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
			REG_T1_COUNTER_LO => {
				self.ifr_t1 = false;
				self.t1_counter_lo
			}
			REG_T1_COUNTER_HI => self.t1_counter_hi,
			REG_T1_LATCH_LO => self.t1_latch_lo,
			REG_T1_LATCH_HI => self.t1_latch_hi,
			REG_IER => self.ier(),
			REG_IFR => self.ifr(),
			_ => {
				error!("Read from unimplemented VIA reg {:01x}", addr);
				0
			}
		}
	}

	pub fn write(&mut self, addr: u16, value: u8) {
		let addr = Self::mask_addr(addr);
		match addr {
			REG_T1_COUNTER_LO | REG_T1_LATCH_LO => {
				self.t1_latch_lo = value;
			},
			REG_T1_LATCH_HI => {
				self.t1_latch_hi = value;
			},
			REG_T1_COUNTER_HI => {
				self.t1_latch_hi = value;
				self.t1_counter_hi = self.t1_latch_hi;
				self.t1_counter_lo = self.t1_latch_lo;
				self.ifr_t1 = false;
				self.t1_pb7 = false;
			},
			REG_IER => self.set_ier(value),
			REG_IFR => self.set_ifr(value),

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
	
	pub fn set_ifr(&mut self, value: u8) {
		self.ifr_t1 = unpack_flag(value, 6);
	}
	
	pub fn set_ier(&mut self, mut value: u8) {
		if !unpack_flag(value, 7) {
			value = !value;
		}
		
		self.ier_t1 = unpack_flag(value, 6);
	}

	pub fn ifr(&self) -> u8 {
		let mut ifr = pack_flags([
			false,
			false,
			false,
			false,
			false,
			false,
			self.ifr_t1,
			false
		]);
		
		// Bit 7 is set whenever any interrupt flag is both active and enabled
		if (ifr & self.ier()) > 0 {
			ifr |= 0b1000_0000;
		}
		
		ifr
	}

	pub fn ier(&self) -> u8 {
		pack_flags([
			false,
			false,
			false,
			false,
			false,
			false,
			self.ier_t1,
			true
		])
	}
}