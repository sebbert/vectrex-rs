#![allow(unused_variables, unused_assignments)]

use motherboard::Motherboard;
use std::fmt::{ self, Debug, Formatter };
use pack::*;

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

impl Debug for Mc6809 {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		fn write_flag(f: &mut Formatter, flag: bool) -> fmt::Result {
			write!(f, "{}", if flag {"*"} else {" "})
		}

		try!(writeln!(f, "DP: {:02x}     A: {:02x}", self.reg_dp(), self.reg_a()));
		try!(writeln!(f, "PC: {:04x}   B: {:02x}", self.reg_pc(), self.reg_b()));
		try!(writeln!(f, " X: {:04x}   D: {:04x}", self.reg_x(), self.reg_d()));
		try!(writeln!(f, " Y: {:04x}", self.reg_y()));
		try!(write!  (f, " S: {:04x}  CC:", self.reg_s()));

		try!(write_flag(f, self.cc_entire_flag));
		try!(write_flag(f, self.cc_firq_mask));
		try!(write_flag(f, self.cc_half_carry));
		try!(write_flag(f, self.cc_irq_mask));
		try!(write_flag(f, self.cc_negative));
		try!(write_flag(f, self.cc_zero));
		try!(write_flag(f, self.cc_overflow));
		try!(write_flag(f, self.cc_carry));

		try!(writeln!(f, ""));
		try!(writeln!(f, " U: {:04x}      EFHINZVC", self.reg_u()));

		Ok(())
	}
}

macro_rules! invalid_opcode {
    ($op:expr) => (panic!("Invalid opcode 0x{:02x}", $op))
}

#[allow(dead_code)]
impl Mc6809 {	
	pub fn new(motherboard: &Motherboard) -> Mc6809 {
		let mut cpu = Mc6809::default();
		
		cpu.cc_irq_mask = true;
		cpu.cc_firq_mask = true;
		
		cpu.reg_pc = motherboard.read_u16(0xfffe);

		cpu
	}

	pub fn reset(&mut self, motherboard: &Motherboard) {
		*self = Mc6809::new(motherboard);
	}

	pub fn step(&mut self, motherboard: &mut Motherboard) -> u32 {
		let mut cycles: u32 = 0;

		let op = motherboard.read_u8(self.reg_pc);
		self.reg_pc = self.reg_pc.wrapping_add(1);

		macro_rules! inherent {
		    ($f:path, $cycles:expr) => ({
		    	cycles += cycles;
		    	$f(self, motherboard);
		    })
		}

		macro_rules! immediate8 {
			($f:path, $cycles:expr) => ({
				let addr = self.reg_pc;
				self.reg_pc = self.reg_pc.wrapping_add(1);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! immediate16 {
			($f:path, $cycles:expr) => ({
				let addr = self.reg_pc;
				self.reg_pc = self.reg_pc.wrapping_add(2);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! direct {
			($f:path, $cycles:expr) => ({
				let addr_lo = motherboard.read_u8(self.reg_pc);
				let addr = pack_u16(self.reg_dp, addr_lo);
				self.reg_pc = self.reg_pc.wrapping_add(1);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! extended {
			($f:path, $cycles:expr) => ({
				let addr = motherboard.read_u16(self.reg_pc);
				self.reg_pc = self.reg_pc.wrapping_add(2);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! indexed {
			($f:path, $cycles:expr) => ({
				let postbyte = motherboard.read_u8(self.reg_pc);
				self.reg_pc = self.reg_pc.wrapping_add(1);
				cycles += $cycles;
				panic!("Indexed addressing is not yet implemented");
			})
		}

		macro_rules! branch8 {
			($f:path) => ({
				cycles += 3;
				
				let should_branch = $f(self);
				self.reg_pc = if should_branch {
					let offset = motherboard.read_u8(self.reg_pc) as i8 as i16;
					offset_address(self.reg_pc, offset)
				} else {
					self.reg_pc.wrapping_add(1)
				}
			})
		}

		macro_rules! branch16 {
			($f:path, $cycles_if_branch:expr, $cycles_if_no_branch:expr) => ({
				let should_branch = $f(self);

				self.reg_pc = if should_branch
				{
					cycles += $cycles_if_branch;
					let offset = motherboard.read_u16(self.reg_pc) as i16;
					offset_address(self.reg_pc, offset)
					
				} else {
					cycles += $cycles_if_no_branch;	
					self.reg_pc.wrapping_add(2)
				}
			});

			($f:path) => (branch16!($f, 6, 5))
		}

		const PAGE_2: u8 = 0x10;
		const PAGE_3: u8 = 0x11;

		match op {
			PAGE_2 => {
				let op = motherboard.read_u8(self.reg_pc);
				self.reg_pc = self.reg_pc.wrapping_add(1);
			
				match op {
					0x83 => immediate16!(Self::instr_cmpd, 5),
					0x93 => direct!(Self::instr_cmpd, 7),
					0xa3 => indexed!(Self::instr_cmpd, 7),
					0xb3 => extended!(Self::instr_cmpd, 8),
					0x8c => immediate16!(Self::instr_cmpy, 5),
					0x9c => direct!(Self::instr_cmpy, 7),
					0xac => indexed!(Self::instr_cmpy, 7),
					0xbc => extended!(Self::instr_cmpy, 8),
					0xce => immediate16!(Self::instr_lds, 4),
					0xde => direct!(Self::instr_lds, 6),
					0xee => indexed!(Self::instr_lds, 6),
					0xfe => extended!(Self::instr_lds, 7),
					0x8e => immediate16!(Self::instr_ldy, 4),
					0x9e => direct!(Self::instr_ldy, 6),
					0xae => indexed!(Self::instr_ldy, 6),
					0xbe => extended!(Self::instr_ldy, 7),
					0x9f => direct!(Self::instr_sty, 6),
					0xaf => indexed!(Self::instr_sty, 6),
					0xbf => extended!(Self::instr_sty, 7),
					0x3f => inherent!(Self::instr_swi2, 20),


					// Branch instructions

					0x20 => branch16!(Self::cond_always, 5, 5),
					0x21 => branch16!(Self::cond_never, 5, 5),
					0x27 => branch16!(Self::cond_equal),
					0x22 => branch16!(Self::cond_unsigned_greater_than),
					0x24 => branch16!(Self::cond_unsigned_greater_than_or_equal),
					0x25 => branch16!(Self::cond_unsigned_less_than),
					0x23 => branch16!(Self::cond_unsigned_less_than_or_equal),
					0x2c => branch16!(Self::cond_signed_greater_than_or_equal),
					0x2e => branch16!(Self::cond_signed_greater_than),
					0x2d => branch16!(Self::cond_signed_less_than),
					0x2f => branch16!(Self::cond_signed_less_than_or_equal),
					0x28 => branch16!(Self::cond_overflow_clear),
					0x29 => branch16!(Self::cond_overflow_set),

					0x17 => {
						// Branch to Subroutine
						cycles += 9;
						let offset = motherboard.read_u16(self.reg_pc) as i16;
						self.reg_pc = self.reg_pc.wrapping_add(2);
						let addr = offset_address(self.reg_pc, offset);
						self.instr_jsr(motherboard, addr);
					}
					_ => invalid_opcode!(op)
				}
			},
			PAGE_3 => {
				let op = motherboard.read_u8(self.reg_pc);
				self.reg_pc = self.reg_pc.wrapping_add(1);
			
				match op {
					0x8c => immediate16!(Self::instr_cmps, 5),
					0x9c => direct!(Self::instr_cmps, 7),
					0xac => indexed!(Self::instr_cmps, 7),
					0xbc => extended!(Self::instr_cmps, 8),
					0x83 => immediate16!(Self::instr_cmpu, 5),
					0x93 => direct!(Self::instr_cmpu, 7),
					0xa3 => indexed!(Self::instr_cmpu, 7),
					0xb3 => extended!(Self::instr_cmpu, 8),
					0x3f => inherent!(Self::instr_swi3, 20),
					_ => invalid_opcode!(op)
				}
			},

			// Branch instructions

			0x20 => branch8!(Self::cond_always),
			0x21 => branch8!(Self::cond_never),
			0x27 => branch8!(Self::cond_equal),
			0x22 => branch8!(Self::cond_unsigned_greater_than),
			0x24 => branch8!(Self::cond_unsigned_greater_than_or_equal),
			0x25 => branch8!(Self::cond_unsigned_less_than),
			0x23 => branch8!(Self::cond_unsigned_less_than_or_equal),
			0x2c => branch8!(Self::cond_signed_greater_than_or_equal),
			0x2e => branch8!(Self::cond_signed_greater_than),
			0x2d => branch8!(Self::cond_signed_less_than),
			0x2f => branch8!(Self::cond_signed_less_than_or_equal),
			0x28 => branch8!(Self::cond_overflow_clear),
			0x29 => branch8!(Self::cond_overflow_set),

			0x8d => {
				// Branch to Subroutine
				cycles += 9;
				let offset = motherboard.read_u8(self.reg_pc) as i8 as i16;
				self.reg_pc = self.reg_pc.wrapping_add(1);
				let addr = offset_address(self.reg_pc, offset);
				self.instr_jsr(motherboard, addr);
			}

			// Regular ordinary swedish instruction time

			0x3a => inherent!(Self::instr_abx, 3),
			0x89 => immediate8!(Self::instr_adca, 2),
			0x99 => direct!(Self::instr_adca, 4),
			0xa9 => indexed!(Self::instr_adca, 4),
			0xb9 => extended!(Self::instr_adca, 5),
			0xc9 => immediate8!(Self::instr_adcb, 2),
			0xd9 => direct!(Self::instr_adcb, 4),
			0xe9 => indexed!(Self::instr_adcb, 4),
			0xf9 => extended!(Self::instr_adcb, 5),
			0x8b => immediate8!(Self::instr_adda, 2),
			0x9b => direct!(Self::instr_adda, 4),
			0xab => indexed!(Self::instr_adda, 4),
			0xbb => extended!(Self::instr_adda, 5),
			0xcb => immediate8!(Self::instr_addb, 2),
			0xdb => direct!(Self::instr_addb, 4),
			0xeb => indexed!(Self::instr_addb, 4),
			0xfb => extended!(Self::instr_addb, 5),
			0xc3 => immediate16!(Self::instr_addd, 4),
			0xd3 => direct!(Self::instr_addd, 6),
			0xe3 => indexed!(Self::instr_addd, 6),
			0xf3 => extended!(Self::instr_addd, 7),
			0x84 => immediate8!(Self::instr_anda, 2),
			0x94 => direct!(Self::instr_anda, 4),
			0xa4 => indexed!(Self::instr_anda, 4),
			0xb4 => extended!(Self::instr_anda, 5),
			0xc4 => immediate8!(Self::instr_andb, 2),
			0xd4 => direct!(Self::instr_andb, 4),
			0xe4 => indexed!(Self::instr_andb, 4),
			0xf4 => extended!(Self::instr_andb, 5),
			0x1c => immediate8!(Self::instr_andcc, 3),
			0x48 => inherent!(Self::instr_asla, 2),
			0x58 => inherent!(Self::instr_aslb, 2),
			0x08 => direct!(Self::instr_asl, 6),
			0x68 => indexed!(Self::instr_asl, 6),
			0x78 => extended!(Self::instr_asl, 7),
			0x47 => inherent!(Self::instr_asra, 2),
			0x57 => inherent!(Self::instr_asrb, 2),
			0x07 => direct!(Self::instr_asr, 6),
			0x67 => indexed!(Self::instr_asr, 6),
			0x77 => extended!(Self::instr_asr, 7),
			0x85 => immediate8!(Self::instr_bita, 2),
			0x95 => direct!(Self::instr_bita, 4),
			0xa5 => indexed!(Self::instr_bita, 4),
			0xb5 => extended!(Self::instr_bita, 5),
			0xc5 => immediate8!(Self::instr_bitb, 2),
			0xd5 => direct!(Self::instr_bitb, 4),
			0xe5 => indexed!(Self::instr_bitb, 4),
			0xf5 => extended!(Self::instr_bitb, 5),
			0x4f => inherent!(Self::instr_clra, 2),
			0x5f => inherent!(Self::instr_clrb, 2),
			0x0f => direct!(Self::instr_clr, 6),
			0x6f => indexed!(Self::instr_clr, 6),
			0x7f => extended!(Self::instr_clr, 7),
			0x81 => immediate8!(Self::instr_cmpa, 2),
			0x91 => direct!(Self::instr_cmpa, 4),
			0xa1 => indexed!(Self::instr_cmpa, 4),
			0xb1 => extended!(Self::instr_cmpa, 5),
			0xc1 => immediate8!(Self::instr_cmpb, 2),
			0xd1 => direct!(Self::instr_cmpb, 4),
			0xe1 => indexed!(Self::instr_cmpb, 4),
			0xf1 => extended!(Self::instr_cmpb, 5),
			0x8c => immediate16!(Self::instr_cmpx, 4),
			0x9c => direct!(Self::instr_cmpx, 6),
			0xac => indexed!(Self::instr_cmpx, 6),
			0xbc => extended!(Self::instr_cmpx, 7),
			0x44 => inherent!(Self::instr_lsra, 2),
			0x54 => inherent!(Self::instr_lsrb, 2),
			0x04 => direct!(Self::instr_lsr, 6),
			0x64 => indexed!(Self::instr_lsr, 6),
			0x74 => extended!(Self::instr_lsr, 7),
			0x3d => inherent!(Self::instr_mul, 11),
			0x40 => inherent!(Self::instr_nega, 2),
			0x50 => inherent!(Self::instr_negb, 2),
			0x00 => direct!(Self::instr_neg, 6),
			0x60 => indexed!(Self::instr_neg, 6),
			0x70 => extended!(Self::instr_neg, 7),
			0x12 => inherent!(Self::instr_nop, 2),
			0x8a => immediate8!(Self::instr_ora, 2),
			0x9a => direct!(Self::instr_ora, 4),
			0xaa => indexed!(Self::instr_ora, 4),
			0xba => extended!(Self::instr_ora, 5),
			0xca => immediate8!(Self::instr_orb, 2),
			0xda => direct!(Self::instr_orb, 4),
			0xea => indexed!(Self::instr_orb, 4),
			0xfa => extended!(Self::instr_orb, 5),
			0x1a => immediate8!(Self::instr_orcc, 3),
			0x34 => immediate8!(Self::instr_pshs, 5),
			0x36 => immediate8!(Self::instr_pshu, 5),
			0x35 => immediate8!(Self::instr_puls, 5),
			0x37 => immediate8!(Self::instr_pulu, 5),
			0x49 => inherent!(Self::instr_rola, 2),
			0x59 => inherent!(Self::instr_rolb, 2),
			0x09 => direct!(Self::instr_rol, 6),
			0x69 => indexed!(Self::instr_rol, 6),
			0x79 => extended!(Self::instr_rol, 7),
			0x46 => inherent!(Self::instr_rora, 2),
			0x56 => inherent!(Self::instr_rorb, 2),
			0x06 => direct!(Self::instr_ror, 6),
			0x66 => indexed!(Self::instr_ror, 6),
			0x76 => extended!(Self::instr_ror, 7),
			0x3b => inherent!(Self::instr_rti, 6),
			0x39 => inherent!(Self::instr_rts, 5),
			0x82 => immediate8!(Self::instr_sbca, 2),
			0x92 => direct!(Self::instr_sbca, 4),
			0xa2 => indexed!(Self::instr_sbca, 4),
			0xb2 => extended!(Self::instr_sbca, 5),
			0xc2 => immediate8!(Self::instr_sbcb, 2),
			0xd2 => direct!(Self::instr_sbcb, 4),
			0xe2 => indexed!(Self::instr_sbcb, 4),
			0xf2 => extended!(Self::instr_sbcb, 5),
			0x1d => inherent!(Self::instr_sex, 2),
			0x43 => inherent!(Self::instr_coma, 2),
			0x53 => inherent!(Self::instr_comb, 2),
			0x03 => direct!(Self::instr_com, 6),
			0x63 => indexed!(Self::instr_com, 6),
			0x73 => extended!(Self::instr_com, 7),
			0x3c => immediate8!(Self::instr_cwai, 22),
			0x19 => inherent!(Self::instr_daa, 2),
			0x4a => inherent!(Self::instr_deca, 2),
			0x5a => inherent!(Self::instr_decb, 2),
			0x0a => direct!(Self::instr_dec, 6),
			0x6a => indexed!(Self::instr_dec, 6),
			0x7a => extended!(Self::instr_dec, 7),
			0x88 => immediate8!(Self::instr_eora, 2),
			0x98 => direct!(Self::instr_eora, 4),
			0xa8 => indexed!(Self::instr_eora, 4),
			0xb8 => extended!(Self::instr_eora, 5),
			0xc8 => immediate8!(Self::instr_eorb, 2),
			0xd8 => direct!(Self::instr_eorb, 4),
			0xe8 => indexed!(Self::instr_eorb, 4),
			0xf8 => extended!(Self::instr_eorb, 5),
			0x1e => immediate8!(Self::instr_exg, 8),
			0x4c => inherent!(Self::instr_inca, 2),
			0x5c => inherent!(Self::instr_incb, 2),
			0x0c => direct!(Self::instr_inc, 6),
			0x6c => indexed!(Self::instr_inc, 6),
			0x7c => extended!(Self::instr_inc, 7),
			0x0e => direct!(Self::instr_jmp, 3),
			0x6e => indexed!(Self::instr_jmp, 3),
			0x7e => extended!(Self::instr_jmp, 4),
			0x9d => direct!(Self::instr_jsr, 7),
			0xad => indexed!(Self::instr_jsr, 7),
			0xbd => extended!(Self::instr_jsr, 8),
			0x86 => immediate8!(Self::instr_lda, 2),
			0x96 => direct!(Self::instr_lda, 4),
			0xa6 => indexed!(Self::instr_lda, 4),
			0xb6 => extended!(Self::instr_lda, 5),
			0xc6 => immediate8!(Self::instr_ldb, 2),
			0xd6 => direct!(Self::instr_ldb, 4),
			0xe6 => indexed!(Self::instr_ldb, 4),
			0xf6 => extended!(Self::instr_ldb, 5),
			0xcc => immediate16!(Self::instr_ldd, 3),
			0xdc => direct!(Self::instr_ldd, 5),
			0xec => indexed!(Self::instr_ldd, 5),
			0xfc => extended!(Self::instr_ldd, 6),
			0xce => immediate16!(Self::instr_ldu, 3),
			0xde => direct!(Self::instr_ldu, 5),
			0xee => indexed!(Self::instr_ldu, 5),
			0xfe => extended!(Self::instr_ldu, 6),
			0x8e => immediate16!(Self::instr_ldx, 3),
			0x9e => direct!(Self::instr_ldx, 5),
			0xae => indexed!(Self::instr_ldx, 5),
			0xbe => extended!(Self::instr_ldx, 6),
			0x32 => indexed!(Self::instr_leas, 4),
			0x33 => indexed!(Self::instr_leau, 4),
			0x30 => indexed!(Self::instr_leax, 4),
			0x31 => indexed!(Self::instr_leay, 4),
			0x97 => direct!(Self::instr_sta, 4),
			0xa7 => indexed!(Self::instr_sta, 4),
			0xb7 => extended!(Self::instr_sta, 5),
			0xd7 => direct!(Self::instr_stb, 4),
			0xe7 => indexed!(Self::instr_stb, 4),
			0xf7 => extended!(Self::instr_stb, 5),
			0xdd => direct!(Self::instr_std, 5),
			0xed => indexed!(Self::instr_std, 5),
			0xfd => extended!(Self::instr_std, 6),
			0xdf => direct!(Self::instr_stu, 5),
			0xef => indexed!(Self::instr_stu, 5),
			0xff => extended!(Self::instr_stu, 6),
			0x9f => direct!(Self::instr_stx, 5),
			0xaf => indexed!(Self::instr_stx, 5),
			0xbf => extended!(Self::instr_stx, 6),
			0x80 => immediate8!(Self::instr_suba, 2),
			0x90 => direct!(Self::instr_suba, 4),
			0xa0 => indexed!(Self::instr_suba, 4),
			0xb0 => extended!(Self::instr_suba, 5),
			0xc0 => immediate8!(Self::instr_subb, 2),
			0xd0 => direct!(Self::instr_subb, 4),
			0xe0 => indexed!(Self::instr_subb, 4),
			0xf0 => extended!(Self::instr_subb, 5),
			0x83 => immediate16!(Self::instr_subd, 4),
			0x93 => direct!(Self::instr_subd, 6),
			0xa3 => indexed!(Self::instr_subd, 6),
			0xb3 => extended!(Self::instr_subd, 7),
			0x3f => inherent!(Self::instr_swi, 19),
			0x13 => inherent!(Self::instr_sync, 2),
			0x1f => immediate8!(Self::instr_tfr1, 6),
			0x4d => inherent!(Self::instr_tsta, 2),
			0x5d => inherent!(Self::instr_tstb, 2),
			0x0d => direct!(Self::instr_tst, 6),
			0x6d => indexed!(Self::instr_tst, 6),
			0x7d => extended!(Self::instr_tst, 7),
			_ => invalid_opcode!(op)
		}

		cycles
	}

	// Simple conditionals

	fn cond_always(&self) -> bool { true }
	fn cond_never(&self) -> bool { false }

	fn cond_equal(&self) -> bool { self.cc_zero }
	fn cond_not_equal(&self) -> bool { !self.cc_zero }

	fn cond_carry_set(&self) -> bool { self.cc_carry }
	fn cond_carry_clear(&self) -> bool { !self.cc_carry }

	fn cond_overflow_set(&self) -> bool { self.cc_overflow }
	fn cond_overflow_clear(&self) -> bool { !self.cc_overflow }

	fn cond_negative(&self) -> bool { self.cc_negative }
	fn cond_positive(&self) -> bool { !self.cc_negative }


	// Signed conditionals

	fn cond_signed_greater_than(&self) -> bool {
		self.cond_not_equal() && self.cond_signed_greater_than_or_equal()
	}

	fn cond_signed_greater_than_or_equal(&self) -> bool {
		self.cc_negative == self.cc_overflow
	}

	fn cond_signed_less_than(&self) -> bool {
		self.cc_negative ^ self.cc_overflow
	}

	fn cond_signed_less_than_or_equal(&self) -> bool {
		self.cond_equal() || self.cond_signed_less_than()
	}


	// Unsigned conditionals

	fn cond_unsigned_greater_than(&self) -> bool {
		self.cond_not_equal() && self.cond_unsigned_greater_than_or_equal()
	}

	fn cond_unsigned_greater_than_or_equal(&self) -> bool {
		self.cond_carry_clear()

	}

	fn cond_unsigned_less_than(&self) -> bool {
		self.cond_carry_set()
	}

	fn cond_unsigned_less_than_or_equal(&self) -> bool {
		self.cond_equal() || self.cond_unsigned_less_than()
	}


	// Instructions

	fn instr_abx(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction ABX");
	}

	fn instr_adca(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ADCA");
	}

	fn instr_adcb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ADCB");
	}

	fn instr_adda(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ADDA");
	}

	fn instr_addb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ADDB");
	}

	fn instr_addd(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ADDD");
	}

	fn instr_anda(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ANDA");
	}

	fn instr_andb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ANDB");
	}

	fn instr_andcc(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ANDCC");
	}

	fn instr_asla(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction ASLA");
	}

	fn instr_aslb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction ASLB");
	}

	fn instr_asl(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ASL");
	}

	fn instr_asra(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction ASRA");
	}

	fn instr_asrb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction ASRB");
	}

	fn instr_asr(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ASR");
	}

	fn instr_bita(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction BITA");
	}

	fn instr_bitb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction BITB");
	}

	fn instr_clra(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction CLRA");
	}

	fn instr_clrb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction CLRB");
	}

	fn instr_clr(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CLR");
	}

	fn instr_cmpa(&mut self, mobo: &Motherboard, addr: u16) {
		let mem = mobo.read_u8(addr);
		let reg = self.reg_a();
		self.sub_u8_and_set_flags(mem, reg);
	}

	fn instr_cmpb(&mut self, mobo: &Motherboard, addr: u16) {
		let mem = mobo.read_u8(addr);
		let reg = self.reg_b();
		self.sub_u8_and_set_flags(mem, reg);
	}

	fn instr_cmpd(&mut self, mobo: &Motherboard, addr: u16) {
		let mem = mobo.read_u16(addr);
		let reg = self.reg_d();
		self.sub_u16_and_set_flags(mem, reg);
	}

	fn instr_cmps(&mut self, mobo: &Motherboard, addr: u16) {
		let mem = mobo.read_u16(addr);
		let reg = self.reg_s();
		self.sub_u16_and_set_flags(mem, reg);
	}

	fn instr_cmpu(&mut self, mobo: &Motherboard, addr: u16) {
		let mem = mobo.read_u16(addr);
		let reg = self.reg_u();
		self.sub_u16_and_set_flags(mem, reg);
	}

	fn instr_cmpx(&mut self, mobo: &Motherboard, addr: u16) {
		let mem = mobo.read_u16(addr);
		let reg = self.reg_x();
		self.sub_u16_and_set_flags(mem, reg);
	}

	fn instr_cmpy(&mut self, mobo: &Motherboard, addr: u16) {
		let mem = mobo.read_u16(addr);
		let reg = self.reg_y();
		self.sub_u16_and_set_flags(mem, reg);
	}

	fn instr_lsra(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction LSRA");
	}

	fn instr_lsrb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction LSRB");
	}

	fn instr_lsr(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LSR");
	}

	fn instr_mul(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction MUL");
	}


	fn instr_nega(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction NEGA");
	}

	fn instr_negb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction NEGB");
	}

	fn instr_neg(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction NEG");
	}

	fn instr_nop(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction NOP");
	}

	fn instr_ora(&mut self, mobo: &Motherboard, addr: u16) {
		self.reg_a |= mobo.read_u8(addr);
	}

	fn instr_orb(&mut self, mobo: &Motherboard, addr: u16) {
		self.reg_b |= mobo.read_u8(addr);
	}

	fn instr_orcc(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ORCC");
	}

	fn instr_pshs(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction PSHS");
	}

	fn instr_pshu(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction PSHU");
	}

	fn instr_puls(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction PULS");
	}

	fn instr_pulu(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction PULU");
	}

	fn instr_rola(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction ROLA");
	}

	fn instr_rolb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction ROLB");
	}

	fn instr_rol(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ROL");
	}

	fn instr_rora(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction RORA");
	}

	fn instr_rorb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction RORB");
	}

	fn instr_ror(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction ROR");
	}

	fn instr_rti(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction RTI");
	}

	fn instr_rts(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction RTS");
	}

	fn instr_sbca(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction SBCA");
	}

	fn instr_sbcb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction SBCB");
	}

	fn instr_sex(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction SEX");
	}

	fn instr_coma(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction COMA");
	}

	fn instr_comb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction COMB");
	}

	fn instr_com(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction COM");
	}

	fn instr_cwai(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CWAI");
	}

	fn instr_daa(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction DAA");
	}

	fn instr_deca(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction DECA");
	}

	fn instr_decb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction DECB");
	}

	fn instr_dec(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction DEC");
	}

	fn instr_eora(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction EORA");
	}

	fn instr_eorb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction EORB");
	}

	fn instr_exg(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction EXG");
	}

	fn instr_inca(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction INCA");
	}

	fn instr_incb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction INCB");
	}

	fn instr_inc(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction INC");
	}

	fn instr_jmp(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction JMP");
	}

	fn instr_jsr(&mut self, mobo: &mut Motherboard, addr: u16) {
		Self::push_u16(&mut self.reg_s, mobo, self.reg_pc);
		self.reg_pc = addr;
	}

	fn instr_lda(&mut self, mobo: &Motherboard, addr: u16) {
		self.reg_a = mobo.read_u8(addr);
	}

	fn instr_ldb(&mut self, mobo: &Motherboard, addr: u16) {
		self.reg_b = mobo.read_u8(addr);
	}

	fn instr_ldd(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LDD");
	}

	fn instr_lds(&mut self, mobo: &Motherboard, address: u16) {
		self.reg_s = mobo.read_u16(address)
	}

	fn instr_ldu(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LDU");
	}

	fn instr_ldx(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LDX");
	}

	fn instr_ldy(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LDY");
	}

	fn instr_leas(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LEAS");
	}

	fn instr_leau(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LEAU");
	}

	fn instr_leax(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LEAX");
	}

	fn instr_leay(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LEAY");
	}

	fn instr_sta(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction STA");
	}

	fn instr_stb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction STB");
	}

	fn instr_std(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction STD");
	}

	fn instr_stu(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction STU");
	}

	fn instr_stx(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction STX");
	}

	fn instr_sty(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction STY");
	}

	fn instr_suba(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction SUBA");
	}

	fn instr_subb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction SUBB");
	}

	fn instr_subd(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction SUBD");
	}

	fn instr_swi(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction SWI");
	}

	fn instr_swi2(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction SWI2");
	}

	fn instr_swi3(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction SWI3");
	}

	fn instr_sync(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction SYNC");
	}

	fn instr_tfr1(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction TFR1");
	}

	fn instr_tsta(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction TSTA");
	}

	fn instr_tstb(&mut self, mobo: &Motherboard) {
		panic!("Unimplemented instruction TSTB");
	}

	fn instr_tst(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction TST");
	}

	fn push_u8(sp: &mut u16, mobo: &mut Motherboard, value: u8) {
		*sp = sp.checked_sub(1).expect("Stack overflow");
		mobo.write_u8(*sp, value);
	}

	fn pop_u8(sp: &mut u16, mobo: &mut Motherboard) -> u8 {
		let value = mobo.read_u8(*sp);
		*sp = sp.checked_sub(1).expect("Stack underflow");
		value
	}

	fn push_u16(sp: &mut u16, mobo: &mut Motherboard, value: u16) {
		let (hi, lo) = unpack_u16(value);

		Self::push_u8(sp, mobo, lo);
		Self::push_u8(sp, mobo, hi);
	}

	fn pop_u16(sp: &mut u16, mobo: &mut Motherboard) -> u16 {
		let hi = Self::pop_u8(sp, mobo);
		let lo = Self::pop_u8(sp, mobo);

		pack_u16(hi, lo)
	}

	fn sub_u8_and_set_flags(&mut self, a: u8, b: u8) {
		let (result, carry) = u8::overflowing_sub(a, b);
		self.cc_carry = carry;
		self.cc_zero = result == 0;
		self.cc_negative = (result & 0b1000) == 0b1000;
	}

	fn sub_u16_and_set_flags(&mut self, a: u16, b: u16) {
		let (result, carry) = u16::overflowing_sub(a, b);
		self.cc_carry = carry;
		self.cc_zero = result == 0;
		self.cc_negative = (result & 0b1000_0000) == 0b1000_0000;
	}

	fn sub_i8_and_set_flags(&mut self, a: i8, b: i8) {
		let (result, overflow) = i8::overflowing_sub(a, b);
		self.cc_overflow = overflow;
		self.cc_zero = result == 0;
		self.cc_negative = result < 0;
	}

	fn sub_i16_and_set_flags(&mut self, a: i16, b: i16) {
		let (result, overflow) = i16::overflowing_sub(a, b);
		self.cc_overflow = overflow;
		self.cc_zero = result == 0;
		self.cc_negative = result < 0;
	}

	pub fn reg_a(&self) -> u8 { self.reg_a }
	pub fn reg_b(&self) -> u8 { self.reg_b }

	pub fn reg_d(&self) -> u16 {
		pack_u16(self.reg_a, self.reg_b)
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
		let (hi, lo) = unpack_u16(value);
		self.reg_a = hi;
		self.reg_b = lo;
	}
	pub fn set_reg_dp(&mut self, value: u8) { self.reg_dp = value }

	pub fn set_reg_x(&mut self, value: u16) { self.reg_x = value }
	pub fn set_reg_y(&mut self, value: u16) { self.reg_y = value }
	pub fn set_reg_u(&mut self, value: u16) { self.reg_u = value }
	pub fn set_reg_s(&mut self, value: u16) { self.reg_s = value }
	pub fn set_reg_pc(&mut self, value: u16) { self.reg_pc = value }
}

fn offset_address(addr: u16, offset: i16) -> u16 {
	if offset >= 0 {
		addr.wrapping_add(offset as u16)
	}
	else {
		addr.wrapping_sub(-offset as u16)
	}
}