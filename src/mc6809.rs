use instruction::*;
use motherboard::Motherboard;

pub enum CcFlag {
	Carry = 0
}

#[derive(Default)]
#[allow(dead_code)]
pub struct Mc6809 {
	reg_a: u8,
	reg_b: u8,
	reg_dp: u8,

	reg_pc: u16,
	reg_x: u16,
	reg_y: u16,
	reg_u: u16,
	reg_s: u16,

	cc_carry: bool,
	cc_overflow: bool,
	cc_zero: bool,
	cc_negative: bool,
	cc_irq_mask: bool,
	cc_half_carry: bool,
	cc_firq_mask: bool,
	cc_entire_flag: bool
}

macro_rules! unknown_opcode {
    ($op:expr) => (panic!("Unknown opcode: 0x{:02x}", $op))
}

#[allow(dead_code)]
impl Mc6809 {	
	pub fn new() -> Mc6809 {
		Mc6809::default()
	}

	pub fn reset(&mut self, motherboard: &Motherboard) {
		self.cc_irq_mask = true;
		self.cc_firq_mask = true;

		// TODO
		self.reg_pc = motherboard.read_u16(0xfffe);
	}

	pub fn step(&mut self, motherboard: &mut Motherboard) -> u32 {
		let mut cycles: u32 = 0;

		let op = motherboard.read_u8(self.reg_pc);
		let mut next_pc = self.reg_pc.wrapping_add(1);

		macro_rules! immediate8 {
			($cycles:expr, $f:expr) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(1);
				cycles += $cycles;
				$f(addr)
			})
		}

		macro_rules! immediate16 {
			($cycles:expr, $f:expr) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(2);
				cycles += $cycles;
				$f(addr)
			})
		}

		macro_rules! direct {
			($cycles:expr, $f:expr) => ({
				let mut addr = motherboard.read_u8(next_pc) as u16;
				addr = addr as u16 | (self.reg_dp as u16) << 8;
				next_pc = next_pc.wrapping_add(1);
				cycles += $cycles;
				$f(addr)
			})
		}

		macro_rules! extended {
			($cycles:expr, $f:expr) => ({
				let addr = motherboard.read_u16(next_pc);
				next_pc = next_pc.wrapping_add(2);
				cycles += $cycles;
				$f(addr)
			})
		}

		macro_rules! indexed {
			($cycles:expr, $f: expr) => ({
				let postbyte = motherboard.read_u8

			})
		}

		match op {
			OP_PAGE_2 => {
				let op = motherboard.read_u8(next_pc);
				next_pc = next_pc.wrapping_add(1);
				match op {
					OP_LDS_IMMEDIATE => immediate16!(4, |addr| self.instr_lds(motherboard, addr)),
					OP_LDS_DIRECT => direct!(6, |addr| self.instr_lds(motherboard, addr)),
					OP_LDS_EXTENDED => extended!(7, |addr| self.instr_lds(motherboard, addr)),

					OP_ORB_IMMEDIATE => immediate8!(2, |addr| self.instr_orb(motherboard, addr)),
					OP_ORB_DIRECT => direct!(4, |addr| self.instr_orb(motherboard, addr)),
					OP_ORB_EXTENDED => extended!(5, |addr| self.instr_orb(motherboard, addr)),

					_ => unknown_opcode!(op)
				}
			},
			OP_PAGE_3 => {
				let op = motherboard.read_u8(next_pc);
				next_pc = next_pc.wrapping_add(1);

				match op {
					_ => unknown_opcode!(op)
				}
			}
			_ => unknown_opcode!(op)
		}

		self.reg_pc = next_pc;

		cycles
	}

	fn instr_lds(&mut self, mobo: &Motherboard, address: u16) {
		self.reg_s = mobo.read_u16(address)
	}

	fn instr_orb(&mut self, mobo: &Motherboard, address: u16) {
		self.reg_b |= mobo.read_u8(address);
	}

	pub fn reg_a(&self) -> u8 { self.reg_a }
	pub fn reg_b(&self) -> u8 { self.reg_b }

	pub fn reg_d(&self) -> u16 {
		((self.reg_b as u16) << 8) | self.reg_a as u16
	}

	pub fn reg_dp(&self) -> u8 { self.reg_dp }

	pub fn reg_x(&self) -> u16 { self.reg_x }
	pub fn reg_y(&self) -> u16 { self.reg_y }
	pub fn reg_u(&self) -> u16 { self.reg_u }
	pub fn reg_s(&self) -> u16 { self.reg_s }
	pub fn reg_pc(&self) -> u16 { self.reg_pc }

	pub fn reg_cc(&self) -> u8 {
		  (self.cc_carry as u8) << 0
		| (self.cc_overflow as u8) << 1
		| (self.cc_zero as u8) << 2
		| (self.cc_negative as u8) << 3
		| (self.cc_irq_mask as u8) << 4
		| (self.cc_half_carry as u8) << 5
		| (self.cc_firq_mask as u8) << 6
		| (self.cc_entire_flag as u8) << 7
	}

	pub fn set_reg_a(&mut self, value: u8) { self.reg_a = value }
	pub fn set_reg_b(&mut self, value: u8) { self.reg_b = value }
	pub fn set_reg_d(&mut self, value: u16) {
		self.reg_b = (value >> 8) as u8;
		self.reg_a = value as u8;
	}
	pub fn set_reg_dp(&mut self, value: u8) { self.reg_dp = value }

	pub fn set_reg_x(&mut self, value: u16) { self.reg_x = value }
	pub fn set_reg_y(&mut self, value: u16) { self.reg_y = value }
	pub fn set_reg_u(&mut self, value: u16) { self.reg_u = value }
	pub fn set_reg_s(&mut self, value: u16) { self.reg_s = value }
	pub fn set_reg_pc(&mut self, value: u16) { self.reg_pc = value }
}