#![allow(unused_variables, unused_assignments)]

use memory::Memory;
use std::fmt::{ self, Display, Formatter };
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

impl Display for Mc6809 {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		try!(writeln!(f, "DP: {:02x}     A: {:02x}", self.reg_dp(), self.reg_a()));
		try!(writeln!(f, "PC: {:04x}   B: {:02x}", self.reg_pc(), self.reg_b()));
		try!(writeln!(f, " X: {:04x}   D: {:04x}", self.reg_x(), self.reg_d()));
		try!(writeln!(f, " Y: {:04x}", self.reg_y()));
		try!(write!  (f, " S: {:04x}  CC:", self.reg_s()));

		fn write_flag(f: &mut Formatter, flag: bool) -> fmt::Result {
			write!(f, "{}", if flag {"*"} else {" "})
		}

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

pub struct Ctx<'a> {
	pub cpu: &'a mut Mc6809,
	pub mem: &'a mut Memory,
}

impl<'a> Ctx<'a> {
	pub fn new(cpu: &'a mut Mc6809, mem: &'a mut Memory) -> Ctx<'a> {
		Ctx { cpu, mem }
	}
}

pub trait Src<D> {
	fn read(self, ctx: &mut Ctx) -> D;
}

pub trait Dst<D> {
	fn write(self, ctx: &mut Ctx, val: D);
}

pub trait SrcDst<D> : Src<D> + Dst<D> {}

impl<T,D> SrcDst<D> for T where T: Src<D> + Dst<D> {}

struct Imm8;
impl Src<u8> for Imm8 {
	fn read(self, ctx: &mut Ctx) -> u8 {
		ctx.cpu.take_8_pc(ctx.mem)
	}
}

struct Imm16;
impl Src<u16> for Imm16 {
	fn read(self, ctx: &mut Ctx) -> u16 {
		ctx.cpu.take_16_pc(ctx.mem)
	}
}

macro_rules! impl_src_dst_for_reg {
	( $typename:ident ( $type:ty ) { $getter:ident , $setter:ident } ) => {
		struct $typename;

		impl Src<$type> for $typename {
			fn read(self, ctx: &mut Ctx) -> $type {
				ctx.cpu.$getter()
			}
		}

		impl Dst<$type> for $typename {
			fn write(self, ctx: &mut Ctx, value: $type) {
				ctx.cpu.$setter(value)
			}
		}
	};
}

impl_src_dst_for_reg! {  A (u8)  { reg_a,  set_reg_a  } }
impl_src_dst_for_reg! {  B (u8)  { reg_b,  set_reg_b  } }
impl_src_dst_for_reg! { DP (u8)  { reg_dp, set_reg_dp } }
impl_src_dst_for_reg! { CC (u8)  { reg_cc, set_reg_cc } }
impl_src_dst_for_reg! {  X (u16) { reg_x,  set_reg_x  } }
impl_src_dst_for_reg! {  Y (u16) { reg_y,  set_reg_y  } }
impl_src_dst_for_reg! {  U (u16) { reg_u,  set_reg_u  } }
impl_src_dst_for_reg! {  S (u16) { reg_s,  set_reg_s  } }
impl_src_dst_for_reg! { PC (u16) { reg_pc, set_reg_pc } }

#[allow(dead_code)]
impl Mc6809 {	
	pub fn new(mem: &mut Memory) -> Mc6809 {
		let mut cpu = Mc6809::default();
		
		cpu.cc_irq_mask = true;
		cpu.cc_firq_mask = true;
		
		cpu.reg_pc = mem.read_16(0xfffe);

		cpu
	}

	pub fn reset(&mut self, mem: &mut Memory) {
		*self = Mc6809::new(mem);
	}

	pub fn step(&mut self, mem: &mut Memory, irq: bool) -> usize {
		let mut cycles: usize = 0;

		if irq && self.cc_irq_mask {
			self.reg_pc = mem.read_16(0xfff8);
		}

		let op = mem.read_8(self.reg_pc);

		if cfg!(feature = "compare") {
			println!("{:x}, {:x}, {:x}, {:x}, {:x}, {:x}, {:x}",
				self.reg_pc,
				op,
				self.reg_a,
				self.reg_b,
				self.reg_cc(),
				self.reg_x,
				self.reg_y);
		}
		
		self.reg_pc = self.reg_pc.wrapping_add(1);

		let ctx = Ctx::new(self, mem);

		macro_rules! inherent {
			($f:expr, $cycles:expr) => ({
				cycles += $cycles;
				$f(ctx.cpu, ctx.mem);
			})
		}

		macro_rules! immediate8 {
			($f:expr, $cycles:expr) => ({
				let addr = ctx.cpu.reg_pc;
				ctx.cpu.reg_pc = ctx.cpu.reg_pc.wrapping_add(1);
				cycles += $cycles;
				$f(ctx.cpu, ctx.mem, addr)
			})
		}

		macro_rules! immediate16 {
			($f:expr, $cycles:expr) => ({
				let addr = ctx.cpu.reg_pc;
				ctx.cpu.reg_pc = ctx.cpu.reg_pc.wrapping_add(2);
				cycles += $cycles;
				$f(ctx.cpu, ctx.mem, addr)
			})
		}

		macro_rules! direct {
			($f:expr, $cycles:expr) => ({
				let addr_lo = ctx.mem.read_8(ctx.cpu.reg_pc);
				let addr = pack_16(ctx.cpu.reg_dp, addr_lo);
				ctx.cpu.reg_pc = ctx.cpu.reg_pc.wrapping_add(1);
				cycles += $cycles;
				$f(ctx.cpu, ctx.mem, addr)
			})
		}

		macro_rules! extended {
			($f:expr, $cycles:expr) => ({
				let addr = ctx.mem.read_16(ctx.cpu.reg_pc);
				ctx.cpu.reg_pc = ctx.cpu.reg_pc.wrapping_add(2);
				cycles += $cycles;
				$f(ctx.cpu, ctx.mem, addr)
			})
		}

		macro_rules! indexed {
			($f:expr, $cycles:expr) => ({
				let (addr, index_cycles) = ctx.cpu.parse_indexed(ctx.mem);
				cycles += $cycles + index_cycles;
				$f(ctx.cpu, ctx.mem, addr)
			})
		}

		macro_rules! branch8 {
			($f:expr) => ({
				cycles += 3;
				
				let should_branch = $f(ctx.cpu);
				ctx.cpu.reg_pc = if should_branch {
					let offset = ctx.cpu.take_8_pc(ctx.mem) as i8 as i16;
					offset_address(ctx.cpu.reg_pc, offset)
				} else {
					ctx.cpu.reg_pc.wrapping_add(1)
				}
			})
		}

		macro_rules! branch16 {
			($f:expr, $cycles_if_branch:expr, $cycles_if_no_branch:expr) => ({
				let should_branch = $f(ctx.cpu);

				ctx.cpu.reg_pc = if should_branch
				{
					cycles += $cycles_if_branch;
					let offset = ctx.mem.read_16(ctx.cpu.reg_pc) as i16;
					offset_address(ctx.cpu.reg_pc, offset)
					
				} else {
					cycles += $cycles_if_no_branch;	
					ctx.cpu.reg_pc.wrapping_add(2)
				}
			});

			($f:expr) => (branch16!($f, 6, 5))
		}

		const PAGE_2: u8 = 0x10;
		const PAGE_3: u8 = 0x11;

		match op {
			PAGE_2 => {
				let op = ctx.cpu.take_8_pc(ctx.mem);
			
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

					0x21 => branch16!(Self::cond_never, 5, 5),
					0x26 => branch16!(Self::cond_not_equal),
					0x27 => branch16!(Self::cond_equal),
					0x22 => branch16!(Self::cond_unsigned_greater_than),
					0x24 => branch16!(Self::cond_unsigned_greater_than_or_equal),
					0x25 => branch16!(Self::cond_unsigned_less_than),
					0x23 => branch16!(Self::cond_unsigned_less_than_or_equal),
					0x2a => branch16!(Self::cond_positive),
					0x2b => branch16!(Self::cond_negative),
					0x2c => branch16!(Self::cond_signed_greater_than_or_equal),
					0x2e => branch16!(Self::cond_signed_greater_than),
					0x2d => branch16!(Self::cond_signed_less_than),
					0x2f => branch16!(Self::cond_signed_less_than_or_equal),
					0x28 => branch16!(Self::cond_overflow_clear),
					0x29 => branch16!(Self::cond_overflow_set),
					_ => ctx.cpu.invalid_opcode(op)
				}
			},
			PAGE_3 => {
				let op = ctx.cpu.take_8_pc(ctx.mem);
			
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
					_ => ctx.cpu.invalid_opcode(op)
				}
			},

			// Branch instructions

			0x20 => branch8!(Self::cond_always),
			0x21 => branch8!(Self::cond_never),
			0x26 => branch8!(Self::cond_not_equal),
			0x27 => branch8!(Self::cond_equal),
			0x22 => branch8!(Self::cond_unsigned_greater_than),
			0x24 => branch8!(Self::cond_unsigned_greater_than_or_equal),
			0x25 => branch8!(Self::cond_unsigned_less_than),
			0x23 => branch8!(Self::cond_unsigned_less_than_or_equal),
			0x2a => branch8!(Self::cond_positive),
			0x2b => branch8!(Self::cond_negative),
			0x2c => branch8!(Self::cond_signed_greater_than_or_equal),
			0x2e => branch8!(Self::cond_signed_greater_than),
			0x2d => branch8!(Self::cond_signed_less_than),
			0x2f => branch8!(Self::cond_signed_less_than_or_equal),
			0x28 => branch8!(Self::cond_overflow_clear),
			0x29 => branch8!(Self::cond_overflow_set),
			0x16 => branch16!(Self::cond_always, 5, 5),

			0x8d | 0x17 => { // BSR and LBSR, respectively
				cycles += if op == 0x8d { 7 } else { 9 };

				let (imm_size, addr_offset) = match op {
					0x8d => (1, ctx.mem.read_8(ctx.cpu.reg_pc) as i8 as i16),
					_    => (2, ctx.mem.read_16(ctx.cpu.reg_pc) as i16)
				};

				ctx.cpu.reg_pc = ctx.cpu.reg_pc.wrapping_add(imm_size);

				let addr = offset_address(ctx.cpu.reg_pc, addr_offset);

				ctx.cpu.instr_jsr(ctx.mem, addr);
			}

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
			0x34 | 0x36 => immediate8!(|cpu: &mut Mc6809, mem: &mut Memory, addr: u16| {
				match op {
					0x34 => cpu.instr_pshs(mem, addr, &mut cycles),
					_x36 => cpu.instr_pshu(mem, addr, &mut cycles),
				}
			}, 5),
			0x35 | 0x37 => immediate8!(|cpu: &mut Mc6809, mem: &mut Memory, addr: u16| {
				match op {
					0x35 => cpu.instr_puls(mem, addr, &mut cycles),
					_x37 => cpu.instr_pulu(mem, addr, &mut cycles),
				}
			}, 5),
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
			0x1f => immediate8!(Self::instr_tfr, 6),
			0x4d => inherent!(Self::instr_tsta, 2),
			0x5d => inherent!(Self::instr_tstb, 2),
			0x0d => direct!(Self::instr_tst, 6),
			0x6d => indexed!(Self::instr_tst, 6),
			0x7d => extended!(Self::instr_tst, 7),
			_ => ctx.cpu.invalid_opcode(op)
		}

		cycles
	}

	/// Returns a tuple of the form (effective_address, cycles)
	fn parse_indexed(&mut self, mem: &mut Memory) -> (u16, usize) {

		let postbyte = self.take_8_pc(mem);

		let indirect = (postbyte >> 4) & 1 == 1;

		macro_rules! indexed_indirect {
			($cycles:expr, $f:expr) => ({
				let mut addr = $f();
				let mut cycles = $cycles;

				if indirect {
					addr = mem.read_16(addr);
					cycles += 3;
				}

				(addr, cycles)
			})
		}
		
		enum IndexRegister { X, Y, U, S }
		
		impl IndexRegister {
			pub fn write(&self, cpu: &mut Mc6809, value: u16) {
				match self {
					&IndexRegister::X => cpu.set_reg_x(value),
					&IndexRegister::Y => cpu.set_reg_y(value),
					&IndexRegister::U => cpu.set_reg_u(value),
					&IndexRegister::S => cpu.set_reg_s(value)
				}
			}
			
			pub fn read(&self, cpu: &Mc6809) -> u16 {	
				match self {
					&IndexRegister::X => cpu.reg_x(),
					&IndexRegister::Y => cpu.reg_y(),
					&IndexRegister::U => cpu.reg_u(),
					&IndexRegister::S => cpu.reg_s()
				}
			}
		}

		let reg_nibble = (postbyte & 0b0110_0000) >> 5;
		let reg = match reg_nibble {
			0 => IndexRegister::X,
			1 => IndexRegister::Y,
			2 => IndexRegister::U,
			3 => IndexRegister::S,
			_ => unreachable!()
		};

		if postbyte >> 7 == 0 {
			let mut offset = postbyte & 0b0001_1111;

			if offset >> 4 == 1 {
				offset |= 0b1110_0000;
			}
			let offset = offset as i8 as i16;
			return (offset_address(reg.read(self), offset), 1);
		}

		fn increment(cpu: &mut Mc6809, reg: IndexRegister, increment_by: i16) -> u16 {
			let addr = reg.read(cpu);
			let new_addr = match increment_by >= 0 {
				true => addr.wrapping_add(increment_by as u16),
				false => addr.wrapping_sub((-increment_by) as u16),
			};

			reg.write(cpu, new_addr);

			addr
		}

		match postbyte & 0b0001_1111 {
			0 => return (increment(self, reg,  1), 2),
			2 => return (increment(self, reg, -1), 2),
			1 => return (increment(self, reg,  2), 3),
			3 => return (increment(self, reg, -2), 3),
			_ => ()
		}
		
		let index_op = postbyte & 0b1111;

		match index_op {
			0b0001 => indexed_indirect!(3, || increment(self, reg, 2)),
			0b0011 => indexed_indirect!(3, || increment(self, reg, -2)),
			0b0100 => indexed_indirect!(0, || reg.read(self)),
			0b0110 => indexed_indirect!(1, || reg.read(self).wrapping_add(self.reg_a as u16)),
			0b0101 => indexed_indirect!(1, || reg.read(self).wrapping_add(self.reg_b as u16)),
			0b1011 => indexed_indirect!(4, || reg.read(self).wrapping_add(self.reg_d())),

			0b1000 => indexed_indirect!(1, || offset_address(reg.read(self), self.take_8_pc(mem) as i8 as i16)),
			0b1001 => indexed_indirect!(4, || offset_address(reg.read(self), self.take_16_pc(mem) as i16)),
			
			0b1100 => indexed_indirect!(1, || offset_address(self.reg_pc, self.take_8_pc(mem) as i8 as i16)),
			0b1101 => indexed_indirect!(5, || offset_address(self.reg_pc, self.take_16_pc(mem) as i16)),

			0b1111 if indirect => (self.take_16_pc(mem), 5),
			
			_ => {
				panic!("Unknown index postbyte op: {:04b}", index_op);
			}
		}
	}

	pub fn take_8_pc(&mut self, mem: &mut Memory) -> u8 {
		let byte = mem.read_8(self.reg_pc);
		self.reg_pc = self.reg_pc.wrapping_add(1);

		byte
	}
	
	pub fn take_16_pc(&mut self, mem: &mut Memory) -> u16 {
		let hi = self.take_8_pc(mem);
		let lo = self.take_8_pc(mem);
		pack_16(hi, lo)
	}

	fn invalid_opcode(&self, op: u8) {
		panic!("Invalid opcode {:02x} at {:04x}", op, self.reg_pc.wrapping_sub(1))
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

	fn instr_abx(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction ABX");
	}

	fn instr_adc(&mut self, mem: &mut Memory, addr: u16, reg: u8) -> u8 {
		let a = mem.read_8(addr);
		let b = reg;
		let result = u16::wrapping_add(a as u16, b as u16);

		self.check_carry_add_8(result);
		self.check_overflow_8(a, b, result as u8);
		self.check_zero_negative_8(result as u8);
		self.check_half_carry_add_8(a, b);

		result as u8
	}

	fn check_half_carry_add_8(&mut self, a: u8, b: u8) {
		self.cc_half_carry = (((a & 0xf) + (b & 0xf)) & 0x10) == 0x10;
	}

	fn instr_adca(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_a;
		self.reg_a = self.instr_adc(mem, addr, reg);
	}

	fn instr_adcb(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_b;
		self.reg_b = self.instr_adc(mem, addr, reg);
	}

	fn instr_adda(&mut self, mem: &mut Memory, addr: u16) {
		let a = self.reg_a;
		let b = mem.read_8(addr);
		let result = self.add_8_and_set_flags(a, b);
		self.reg_a = result;
		self.check_half_carry_add_8(a, b);
	}

	fn instr_addb(&mut self, mem: &mut Memory, addr: u16) {
		let a = self.reg_b;
		let b = mem.read_8(addr);
		let result = self.add_8_and_set_flags(a, b);
		self.reg_b = result;
		self.check_half_carry_add_8(a, b);
	}

	fn instr_addd(&mut self, mem: &mut Memory, addr: u16) {
		let a = self.reg_d();
		let b = mem.read_16(addr);
		let result = self.add_16_and_set_flags(a, b);
		self.set_reg_d(result);
	}

	fn and(&mut self, a: u8, b: u8) -> u8 {
		let result = a & b;
		self.check_zero_negative_reset_overflow_8(result);
		result
	}

	fn instr_anda(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_a;
		let result = self.and(reg, mem.read_8(addr));
		self.reg_a = result;
	}

	fn instr_andb(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_b;
		let result = self.and(reg, mem.read_8(addr));
		self.reg_b = result;
	}

	fn instr_andcc(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.reg_cc();
		let value = self.and(value, mem.read_8(addr));
		self.set_reg_cc(value);
	}

	fn asl(&mut self, value: u8) -> u8 {
		let value_16 = value as u16;
		let result = value_16 + value_16;
		self.check_overflow_8(value, value, result as u8);
		self.check_carry_add_8(result);
		self.check_zero_negative_8(result as u8);

		result as u8
	}

	fn asr(&mut self, value: u8) -> u8 {
		let value_16 = value as u16;
		let result = (value_16 >> 1) | value_16 & 0x80;

		self.check_zero_negative_8(result as u8);
		self.cc_carry = value.get_flag(0);

		result as u8
	}

	fn instr_asla(&mut self, mem: &mut Memory) {
		let reg = self.reg_a;
		self.reg_a = self.asl(reg);
	}

	fn instr_aslb(&mut self, mem: &mut Memory) {
		let reg = self.reg_b;
		self.reg_b = self.asl(reg);
	}

	fn instr_asl(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.asl(mem.read_8(addr));
		mem.write_8(addr, value);
	}

	fn instr_asra(&mut self, mem: &mut Memory) {
		let reg = self.reg_a;
		self.reg_a = self.asr(reg);
	}

	fn instr_asrb(&mut self, mem: &mut Memory) {
		let reg = self.reg_b;
		self.reg_b = self.asr(reg);
	}

	fn instr_asr(&mut self, mem: &mut Memory, addr: u16) {
		let result = self.asr(mem.read_8(addr));
		mem.write_8(addr, result);
	}

	fn instr_bit(&mut self, mem: &mut Memory, addr: u16, reg: u8) {
		let mem = mem.read_8(addr);
		self.check_zero_negative_reset_overflow_8(mem & reg);
	}

	fn instr_bita(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_a;
		self.instr_bit(mem, addr, reg);
	}

	fn instr_bitb(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_b;
		self.instr_bit(mem, addr, reg);
	} 

	fn instr_clr_flags(&mut self) {
		self.check_zero_negative_8(0);
		self.cc_overflow = false;
		self.cc_carry = false;
	}

	fn instr_clra(&mut self, mem: &mut Memory) {
		self.instr_clr_flags();
		self.reg_a = 0;
	}

	fn instr_clrb(&mut self, mem: &mut Memory) {
		self.instr_clr_flags();
		self.reg_b = 0;
	}

	fn instr_clr(&mut self, mem: &mut Memory, addr: u16) {
		self.instr_clr_flags();
		mem.write_8(addr, 0);
	}

	fn instr_cmpa(&mut self, mem: &mut Memory, addr: u16) {
		let mem = mem.read_8(addr);
		let reg = self.reg_a();
		self.sub_8_and_set_flags(reg, mem);
	}

	fn instr_cmpb(&mut self, mem: &mut Memory, addr: u16) {
		let mem = mem.read_8(addr);
		let reg = self.reg_b();
		self.sub_8_and_set_flags(reg, mem);
	}

	fn instr_cmpd(&mut self, mem: &mut Memory, addr: u16) {
		let mem = mem.read_16(addr);
		let reg = self.reg_d();
		self.sub_16_and_set_flags(reg, mem);
	}

	fn instr_cmps(&mut self, mem: &mut Memory, addr: u16) {
		let mem = mem.read_16(addr);
		let reg = self.reg_s();
		self.sub_16_and_set_flags(reg, mem);
	}

	fn instr_cmpu(&mut self, mem: &mut Memory, addr: u16) {
		let mem = mem.read_16(addr);
		let reg = self.reg_u();
		self.sub_16_and_set_flags(reg, mem);
	}

	fn instr_cmpx(&mut self, mem: &mut Memory, addr: u16) {
		let mem = mem.read_16(addr);
		let reg = self.reg_x();
		self.sub_16_and_set_flags(reg, mem);
	}

	fn instr_cmpy(&mut self, mem: &mut Memory, addr: u16) {
		let mem = mem.read_16(addr);
		let reg = self.reg_y();
		self.sub_16_and_set_flags(reg, mem);
	}

	fn lsr(&mut self, value: u8) -> u8 {
		let shifted = value >> 1;
		self.check_zero_negative_8(shifted);
		self.cc_carry = value.get_flag(0);
		shifted
	}

	fn instr_lsra(&mut self, mem: &mut Memory) {
		let value = self.reg_a;
		let value = self.lsr(value);
		self.reg_a = value;
	}

	fn instr_lsrb(&mut self, mem: &mut Memory) {
		let value = self.reg_b;
		let value = self.lsr(value);
		self.reg_b = value;
	}

	fn instr_lsr(&mut self, mem: &mut Memory, addr: u16) {
		let value = mem.read_8(addr);
		let value = self.lsr(value);
		mem.write_8(addr, value);
	}

	fn instr_mul(&mut self, mem: &mut Memory) {
		let a = self.reg_a as u16;
		let b = self.reg_b as u16;
		let result = a * b;
		self.cc_carry = result.get_flag(7);
		self.set_reg_d(result);
	}

	fn neg_8_and_set_flags(&mut self, value: u8) -> u8 {
		self.add_8_and_set_flags(!value, 1)
	}

	fn instr_nega(&mut self, mem: &mut Memory) {
		let reg = self.reg_a;
		self.reg_a = self.neg_8_and_set_flags(reg);
	}

	fn instr_negb(&mut self, mem: &mut Memory) {
		let reg = self.reg_b;
		self.reg_b = self.neg_8_and_set_flags(reg);
	}

	fn instr_neg(&mut self, mem: &mut Memory, addr: u16) {
		let value = mem.read_8(addr);
		let value = self.neg_8_and_set_flags(value);
		mem.write_8(addr, value);
	}

	fn instr_nop(&mut self, mem: &mut Memory) {
	}

	fn instr_ora(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_a |= mem.read_8(addr);
	}

	fn instr_orb(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_b |= mem.read_8(addr);
	}

	fn instr_orcc(&mut self, mem: &mut Memory, addr: u16) {
		panic!("Unimplemented instruction ORCC");
	}

	fn instr_psh(&self, mem: &mut Memory, postbyte: u8, sp: &mut u16, cycles: &mut usize) {
		*cycles += postbyte.count_ones() as usize;
		let original_sp = *sp;
		if postbyte.get_flag(7) { Self::push_16(sp, mem, self.reg_pc) }
		if postbyte.get_flag(6) { Self::push_16(sp, mem, original_sp) }
		if postbyte.get_flag(5) { Self::push_16(sp, mem, self.reg_y) }
		if postbyte.get_flag(4) { Self::push_16(sp, mem, self.reg_x) }
		if postbyte.get_flag(3) { Self::push_8(sp, mem, self.reg_dp) }
		if postbyte.get_flag(2) { Self::push_8(sp, mem, self.reg_b) }
		if postbyte.get_flag(1) { Self::push_8(sp, mem, self.reg_a) }
		if postbyte.get_flag(0) { Self::push_8(sp, mem, self.reg_cc()) }
	}

	fn instr_pshs(&mut self, mem: &mut Memory, addr: u16, cycles: &mut usize) {
		let mut new_s = self.reg_s;
		let postbyte = mem.read_8(addr);
		self.instr_psh(mem, postbyte, &mut new_s, cycles);
		self.reg_s = new_s;
	}

	fn instr_pshu(&mut self, mem: &mut Memory, addr: u16, cycles: &mut usize) {
		let mut new_u = self.reg_u;
		let postbyte = mem.read_8(addr);
		self.instr_psh(mem, postbyte, &mut new_u, cycles);
		self.reg_u = new_u;
	}

	fn instr_pul(&mut self, mem: &mut Memory, postbyte: u8, sp: &mut u16, cycles: &mut usize) {
		*cycles += postbyte.count_ones() as usize;
		if postbyte.get_flag(0) { self.set_reg_cc(Self::pop_8(sp, mem)); }
		if postbyte.get_flag(1) { self.reg_a = Self::pop_8(sp, mem); }
		if postbyte.get_flag(2) { self.reg_b = Self::pop_8(sp, mem); }
		if postbyte.get_flag(3) { self.reg_dp = Self::pop_8(sp, mem); }
		if postbyte.get_flag(4) { self.reg_x = Self::pop_16(sp, mem); }
		if postbyte.get_flag(5) { self.reg_y = Self::pop_16(sp, mem); }
		if postbyte.get_flag(6) { *sp = Self::pop_16(sp, mem); }
		if postbyte.get_flag(7) { self.reg_pc = Self::pop_16(sp, mem); }
	}

	fn instr_puls(&mut self, mem: &mut Memory, addr: u16, cycles: &mut usize) {
		let mut new_s = self.reg_s;
		let postbyte = mem.read_8(addr);
		self.instr_pul(mem, postbyte, &mut new_s, cycles);
		self.reg_s = new_s;
	}

	fn instr_pulu(&mut self, mem: &mut Memory, addr: u16, cycles: &mut usize) {
		let mut new_u = self.reg_u;
		let postbyte = mem.read_8(addr);
		self.instr_pul(mem, postbyte, &mut new_u, cycles);
		self.reg_u = new_u;
	}

	fn instr_rola(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction ROLA");
	}

	fn instr_rolb(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction ROLB");
	}

	fn instr_rol(&mut self, mem: &mut Memory, addr: u16) {
		panic!("Unimplemented instruction ROL");
	}

	fn instr_rora(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction RORA");
	}

	fn instr_rorb(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction RORB");
	}

	fn instr_ror(&mut self, mem: &mut Memory, addr: u16) {
		panic!("Unimplemented instruction ROR");
	}

	fn instr_rti(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction RTI");
	}

	fn instr_rts(&mut self, mem: &mut Memory) {
		self.reg_pc = Self::pop_16(&mut self.reg_s, mem);
	}

	fn instr_sbc(&mut self, mem: &mut Memory, addr: u16, reg: u8) -> u8 {
		let r = reg as i16;
		let m = mem.read_8(addr) as i16;
		let result = r.wrapping_sub(m).wrapping_sub(self.cc_carry as u8 as i16);
		
		self.check_zero_negative_16(result as u16);
		self.cc_carry = result < 0;
		
		result as u8
	}

	fn instr_sbca(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_a;
		self.reg_a = self.instr_sbc(mem, addr, reg);
	}

	fn instr_sbcb(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_b;
		self.reg_b = self.instr_sbc(mem, addr, reg);
	}

	fn instr_sex(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction SEX");
	}

	fn com_and_set_flags(&mut self, value: u8) -> u8 {
		let value = !value;
 
		self.cc_carry = true;
		self.cc_overflow = false;
		self.cc_negative = (value >> 7) == 1;
		self.cc_zero = value == 0;

		value
	}

	fn instr_coma(&mut self, mem: &mut Memory) {
		let value = self.reg_a;
		self.reg_a = self.com_and_set_flags(value);
	}

	fn instr_comb(&mut self, mem: &mut Memory) {
		let value = self.reg_a;
		self.reg_a = self.com_and_set_flags(value);
	}

	fn instr_com(&mut self, mem: &mut Memory, addr: u16) {
		let value = mem.read_8(addr);
		mem.write_8(addr, self.com_and_set_flags(value));
	}

	fn instr_cwai(&mut self, mem: &mut Memory, addr: u16) {
		panic!("Unimplemented instruction CWAI");
	}

	fn instr_daa(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction DAA");
	}
	
	fn instr_dec_impl(&mut self, value: u8) -> u8 {
		let result = u8::wrapping_sub(value, 1);
		self.check_overflow_8(value, 1, result);
		self.check_zero_negative_8(result);
		
		result
	}
	
	fn instr_deca(&mut self, mem: &mut Memory) {
		let reg = self.reg_a;
		self.reg_a = self.instr_dec_impl(reg);
	}

	fn instr_decb(&mut self, mem: &mut Memory) {
		let reg = self.reg_b;
		self.reg_b = self.instr_dec_impl(reg);
	}

	fn instr_dec(&mut self, mem: &mut Memory, addr: u16) {
		let value = mem.read_8(addr);
		mem.write_8(addr, self.instr_dec_impl(value));
	}

	fn instr_eor(&mut self, mem: &mut Memory, addr: u16, value: u8) -> u8 {
		let value = value ^ mem.read_8(addr);
		self.cc_overflow = false;
		self.check_zero_negative_8(value);

		value
	}

	fn instr_eora(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_a;
		self.reg_a = self.instr_eor(mem, addr, reg);
	}

	fn instr_eorb(&mut self, mem: &mut Memory, addr: u16) {
		let reg = self.reg_b;
		self.reg_b = self.instr_eor(mem, addr, reg);
	}

	fn inc(&mut self, value: u8) -> u8 {
		let result = value.wrapping_add(1);
		self.check_zero_negative_reset_overflow_8(result);
		self.check_overflow_8(value, 1, result);
		result
	}

	fn instr_inca(&mut self, mem: &mut Memory) {
		let value = self.reg_a;
		let value = self.inc(value);
		self.reg_a = value;
	}

	fn instr_incb(&mut self, mem: &mut Memory) {
		let value = self.reg_b;
		let value = self.inc(value);
		self.reg_b = value;
	}

	fn instr_inc(&mut self, mem: &mut Memory, addr: u16) {
		let value = mem.read_8(addr);
		let value = self.inc(value);
		mem.write_8(addr, value);
	}

	fn instr_jmp(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_pc = addr;
	}

	fn instr_jsr(&mut self, mem: &mut Memory, addr: u16) {
		Self::push_16(&mut self.reg_s, mem, self.reg_pc);
		self.reg_pc = addr;
	}

	fn instr_ld_8(&mut self, mem: &mut Memory, addr: u16) -> u8 {
		let value = mem.read_8(addr);
		self.check_zero_negative_reset_overflow_8(value);
		value
	}

	fn instr_ld_16(&mut self, mem: &mut Memory, addr: u16) -> u16 {
		let value = mem.read_16(addr);
		self.check_zero_negative_reset_overflow_16(value);
		value
	}

	fn instr_lda(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_a = self.instr_ld_8(mem, addr);
	}

	fn instr_ldb(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_b = self.instr_ld_8(mem, addr);
	}

	fn instr_ldd(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.instr_ld_16(mem, addr);
		self.set_reg_d(value);
	}

	fn instr_lds(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_s = self.instr_ld_16(mem, addr);
	}

	fn instr_ldu(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_u = self.instr_ld_16(mem, addr);
	}

	fn instr_ldx(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_x = self.instr_ld_16(mem, addr);
	}

	fn instr_ldy(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_y = self.instr_ld_16(mem, addr);
	}

	fn instr_leas(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_s = addr;
	}

	fn instr_leau(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_u = addr;
	}

	fn instr_leax(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_x = addr;
		self.cc_negative = 0 == addr;
	}

	fn instr_leay(&mut self, mem: &mut Memory, addr: u16) {
		self.reg_y = addr;
		self.cc_negative = 0 == addr;
	}

	fn instr_st_8(&mut self, mem: &mut Memory, addr: u16, value: u8) {
		self.check_zero_negative_reset_overflow_8(value);
		mem.write_8(addr, value);
	}

	fn instr_st_16(&mut self, mem: &mut Memory, addr: u16, value: u16) {
		self.check_zero_negative_reset_overflow_16(value);
		mem.write_16(addr, value);
	}

	fn instr_sta(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.reg_a;
		self.instr_st_8(mem, addr, value);
	}

	fn instr_stb(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.reg_b;
		self.instr_st_8(mem, addr, value);
	}

	fn instr_std(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.reg_d();
		self.instr_st_16(mem, addr, value);
	}

	fn instr_stu(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.reg_u;
		self.instr_st_16(mem, addr, value);
	}

	fn instr_stx(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.reg_x;
		self.instr_st_16(mem, addr, value);
	}

	fn instr_sty(&mut self, mem: &mut Memory, addr: u16) {
		let value = self.reg_y;
		self.instr_st_16(mem, addr, value);
	}

	fn instr_suba(&mut self, mem: &mut Memory, addr: u16) {
		let m = self.reg_a;
		let s = mem.read_8(addr);
		self.reg_a = self.sub_8_and_set_flags(m, s);
	}

	fn instr_subb(&mut self, mem: &mut Memory, addr: u16) {
		let m = self.reg_b;
		let s = mem.read_8(addr);
		self.reg_b = self.sub_8_and_set_flags(m, s);
	}

	fn instr_subd(&mut self, mem: &mut Memory, addr: u16) {
		let m = self.reg_d();
		let s = mem.read_16(addr);
		let result = self.sub_16_and_set_flags(m, s);
		self.set_reg_d(result);
	}

	fn instr_swi(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction SWI");
	}

	fn instr_swi2(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction SWI2");
	}

	fn instr_swi3(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction SWI3");
	}

	fn instr_sync(&mut self, mem: &mut Memory) {
		panic!("Unimplemented instruction SYNC");
	}

	fn get_tfr_reg_8(&self, reg: u8) -> u8 {
		match reg {
			0b1000 => self.reg_a(),
			0b1001 => self.reg_b(),
			0b1010 => self.reg_cc(),
			0b1011 => self.reg_dp(),
			_ => panic!("Invalid transfer reg {:04b}", reg)
		}
	}

	fn set_tfr_reg_8(&mut self, reg: u8, value: u8) {
		match reg {
			0b1000 => self.set_reg_a(value),
			0b1001 => self.set_reg_b(value),
			0b1010 => self.set_reg_cc(value),
			0b1011 => self.set_reg_dp(value),
			_ => panic!("Invalid transfer reg {:04b}", reg)
		}
	}

	fn get_tfr_reg_16(&self, reg: u8) -> u16 {
		match reg {
			0b0000 => self.reg_d(),
			0b0001 => self.reg_x(),
			0b0010 => self.reg_y(),
			0b0011 => self.reg_u(),
			0b0100 => self.reg_s(),
			0b0101 => self.reg_pc(),
			_ => panic!("Invalid transfer reg {:04b}", reg)
		}
	}

	fn set_tfr_reg_16(&mut self, reg: u8, value: u16) {
		match reg {
			0b0000 => self.set_reg_d(value),
			0b0001 => self.set_reg_x(value),
			0b0010 => self.set_reg_y(value),
			0b0011 => self.set_reg_u(value),
			0b0100 => self.set_reg_s(value),
			0b0101 => self.set_reg_pc(value),
			_ => panic!("Invalid transfer reg {:04b}", reg)
		}
	}

	fn is_tfr_reg_16(reg: u8) -> bool {
		(reg & 0b1000) >> 3 == 0
	}

	fn instr_tfr(&mut self, mem: &mut Memory, addr: u16) {
		let postbyte = mem.read_8(addr);

		let src_reg = postbyte >> 4;
		let dst_reg = postbyte & 0b1111;

		let is_dst_16 = Self::is_tfr_reg_16(dst_reg);
		let is_src_16 = Self::is_tfr_reg_16(src_reg);

		if is_dst_16 != is_src_16 {
			warn!("Attempted TFR between registers of different size, ignoring");
			return;
		}

		match is_dst_16 {
			true => {
				let src_value = self.get_tfr_reg_16(src_reg);
				self.set_tfr_reg_16(dst_reg, src_value);
			},
			false => {
				let src_value = self.get_tfr_reg_8(src_reg);
				self.set_tfr_reg_8(dst_reg, src_value);
			}
		}
	}

	fn instr_exg(&mut self, mem: &mut Memory, addr: u16) {
		let postbyte = mem.read_8(addr);

		let reg1 = postbyte >> 4;
		let reg2 = postbyte & 0b1111;

		let is_reg1_16 = Self::is_tfr_reg_16(reg1);
		let is_reg2_16 = Self::is_tfr_reg_16(reg2);

		if is_reg1_16 != is_reg2_16 {
			warn!("Attempted EXG between registers of different size, ignoring");
			return;
		}

		match is_reg1_16 {
			true => {
				let reg1_value = self.get_tfr_reg_16(reg1);
				let reg2_value = self.get_tfr_reg_16(reg2);
				self.set_tfr_reg_16(reg1, reg2_value);
				self.set_tfr_reg_16(reg2, reg1_value);
			},
			false => {
				let reg1_value = self.get_tfr_reg_8(reg1);
				let reg2_value = self.get_tfr_reg_8(reg2);
				self.set_tfr_reg_8(reg1, reg2_value);
				self.set_tfr_reg_8(reg2, reg1_value);
			}
		}
	}

	fn instr_tsta(&mut self, mem: &mut Memory) {
		let reg = self.reg_a;
		self.check_zero_negative_reset_overflow_8(reg);
	}

	fn instr_tstb(&mut self, mem: &mut Memory) {
		let reg = self.reg_b;
		self.check_zero_negative_reset_overflow_8(reg);
	}

	fn instr_tst(&mut self, mem: &mut Memory, addr: u16) {
		self.check_zero_negative_reset_overflow_8(mem.read_8(addr));
	}

	fn push_8(sp: &mut u16, mem: &mut Memory, value: u8) {
		*sp = sp.checked_sub(1).expect("Stack overflow");
		mem.write_8(*sp, value);
	}

	fn pop_8(sp: &mut u16, mem: &mut Memory) -> u8 {
		let value = mem.read_8(*sp);
		*sp = sp.checked_add(1).expect("Stack underflow");
		value
	}

	fn push_16(sp: &mut u16, mem: &mut Memory, value: u16) {
		let (hi, lo) = unpack_16(value);

		Self::push_8(sp, mem, lo);
		Self::push_8(sp, mem, hi);
	}

	fn pop_16(sp: &mut u16, mem: &mut Memory) -> u16 {
		let hi = Self::pop_8(sp, mem);
		let lo = Self::pop_8(sp, mem);

		pack_16(hi, lo)
	}

	fn check_zero_negative_reset_overflow_8(&mut self, value: u8) {
		self.cc_overflow = false;
		self.check_zero_negative_8(value);
	}
	
	fn check_zero_negative_reset_overflow_16(&mut self, value: u16) {
		self.cc_overflow = false;
		self.check_zero_negative_16(value);
	}

	fn check_zero_negative_8(&mut self, value: u8) {
		self.cc_zero = value == 0;
		self.cc_negative = (value as i8) < 0;
	}

	fn check_zero_negative_16(&mut self, value: u16) {
		self.cc_zero = value == 0;
		self.cc_negative = (value as i16) < 0;
	}
	
	fn check_overflow_8(&mut self, a: u8, b: u8, r: u8) {
		self.cc_overflow = (!(a^b) & (a^r)).get_flag(7)
	}
	
	fn check_carry_add_8(&mut self, result: u16) {
		self.cc_carry = !result.get_flag(8);
	}
	
	fn check_carry_add_16(&mut self, result: u32) {
		self.check_carry_add_8((result >> 8) as u16);
	}
	
	fn check_carry_sub_8(&mut self, result: u16) {
		self.check_carry_add_8(result);
		self.cc_carry = !self.cc_carry;
	}
	
	fn check_carry_sub_16(&mut self, result: u32) {
		self.check_carry_sub_8((result >> 8) as u16);
	}

	fn sub_8_and_set_flags(&mut self, a: u8, b: u8) -> u8 {
		let a = a as u16;
		let b = b as u16;
		let result = u16::wrapping_sub(a, b);
		self.check_carry_sub_8(result);
		self.check_overflow_8(a as u8, b as u8, result as u8);
		self.check_zero_negative_8(result as u8);
		result as u8
	}

	fn sub_16_and_set_flags(&mut self, a: u16, b: u16) -> u16 {
		let a = a as u32;
		let b = (!(b as u32)).wrapping_add(1);
		let result = u32::wrapping_add(a, b);
		self.check_carry_sub_16(result);
		self.check_overflow_8((a >> 8) as u8, (b >> 8) as u8, (result >> 8) as u8);
		self.check_zero_negative_16(result as u16);
		result as u16
	}

	fn add_8_and_set_flags(&mut self, a: u8, b: u8) -> u8 {
		let a = a as u16;
		let b = b as u16;
		let result = u16::wrapping_add(a, b);
		self.check_carry_add_8(result);
		self.check_overflow_8(a as u8, b as u8, result as u8);
		self.check_zero_negative_8(result as u8);
		result as u8
	}

	fn add_16_and_set_flags(&mut self, a: u16, b: u16) -> u16 {
		let a = a as u32;
		let b = b as u32;
		let result = u32::wrapping_add(a, b);
		self.check_carry_add_16(result);
		self.check_overflow_8((a >> 8) as u8, (b >> 8) as u8, (result >> 8) as u8);
		self.check_zero_negative_16(result as u16);
		result as u16
	}

	pub fn reg_a(&self) -> u8 { self.reg_a }
	pub fn reg_b(&self) -> u8 { self.reg_b }

	pub fn reg_d(&self) -> u16 {
		pack_16(self.reg_a, self.reg_b)
	}

	pub fn reg_dp(&self) -> u8 { self.reg_dp }

	pub fn reg_x(&self) -> u16 { self.reg_x }
	pub fn reg_y(&self) -> u16 { self.reg_y }
	pub fn reg_u(&self) -> u16 { self.reg_u }
	pub fn reg_s(&self) -> u16 { self.reg_s }
	pub fn reg_pc(&self) -> u16 { self.reg_pc }

	pub fn reg_cc(&self) -> u8 {
		pack_flags([
			self.cc_carry,
			self.cc_overflow,
			self.cc_zero,
			self.cc_negative,
			self.cc_irq_mask,
			self.cc_half_carry,
			self.cc_firq_mask,
			self.cc_entire_flag
		])
	}

	pub fn set_reg_cc(&mut self, value: u8) {
		self.cc_carry       = value.get_flag(0);
		self.cc_overflow    = value.get_flag(1);
		self.cc_zero        = value.get_flag(2);
		self.cc_negative    = value.get_flag(3);
		self.cc_irq_mask    = value.get_flag(4);
		self.cc_half_carry  = value.get_flag(5);
		self.cc_firq_mask   = value.get_flag(6);
		self.cc_entire_flag = value.get_flag(7);
	}

	pub fn set_reg_a(&mut self, value: u8) { self.reg_a = value }
	pub fn set_reg_b(&mut self, value: u8) { self.reg_b = value }
	pub fn set_reg_d(&mut self, value: u16) {
		let (hi, lo) = unpack_16(value);
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
	addr.wrapping_add(offset as u16)
}