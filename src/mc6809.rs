#![allow(unused_variables, dead_code, unused_assignments)]

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

macro_rules! invalid_opcode {
    ($op:expr) => (panic!("Invalid opcode 0x{:02x}", $op))
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

		macro_rules! inherent {
		    ($cycles:expr, $f:path) => ({
		    	cycles += cycles;
		    	$f(self, motherboard);
		    })
		}

		macro_rules! immediate8 {
			($cycles:expr, $f:path) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(1);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! immediate16 {
			($cycles:expr, $f:path) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(2);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! immediate24 {
			($cycles:expr, $f:path) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(3);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! direct {
			($cycles:expr, $f:path) => ({
				let mut addr = motherboard.read_u8(next_pc) as u16;
				addr = addr as u16 | (self.reg_dp as u16) << 8;
				next_pc = next_pc.wrapping_add(1);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! extended {
			($cycles:expr, $f:expr) => ({
				let addr = motherboard.read_u16(next_pc);
				next_pc = next_pc.wrapping_add(2);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! indexed {
			($cycles:expr, $f: expr) => ({
				let postbyte = motherboard.read_u8(next_pc);
				next_pc = next_pc.wrapping_add(1);
				cycles += $cycles;
				panic!("Indexed addressing is not yet implemented");
			})
		}

		match op {
			OP_PAGE_2 => {
				let op = motherboard.read_u8(next_pc);
				next_pc = next_pc.wrapping_add(1);
			
				match op {
					OP_CMPD_IMMEDIATE => immediate24!(5, Self::instr_cmpd),
					OP_CMPD_DIRECT => direct!(7, Self::instr_cmpd),
					OP_CMPD_INDEXED => indexed!(7, Self::instr_cmpd),
					OP_CMPD_EXTENDED => extended!(8, Self::instr_cmpd),
					OP_CMPY_IMMEDIATE => immediate24!(5, Self::instr_cmpy),
					OP_CMPY_DIRECT => direct!(7, Self::instr_cmpy),
					OP_CMPY_INDEXED => indexed!(7, Self::instr_cmpy),
					OP_CMPY_EXTENDED => extended!(8, Self::instr_cmpy),
					OP_LDS_IMMEDIATE => immediate24!(4, Self::instr_lds),
					OP_LDS_DIRECT => direct!(6, Self::instr_lds),
					OP_LDS_INDEXED => indexed!(6, Self::instr_lds),
					OP_LDS_EXTENDED => extended!(7, Self::instr_lds),
					OP_LDY_IMMEDIATE => immediate24!(4, Self::instr_ldy),
					OP_LDY_DIRECT => direct!(6, Self::instr_ldy),
					OP_LDY_INDEXED => indexed!(6, Self::instr_ldy),
					OP_LDY_EXTENDED => extended!(7, Self::instr_ldy),
					OP_STY_DIRECT => direct!(6, Self::instr_sty),
					OP_STY_INDEXED => indexed!(6, Self::instr_sty),
					OP_STY_EXTENDED => extended!(7, Self::instr_sty),
					OP_SWI2 => inherent!(20, Self::instr_swi2),
					_ => invalid_opcode!(op)
				}
			},
			OP_PAGE_3 => {
				let op = motherboard.read_u8(next_pc);
				next_pc = next_pc.wrapping_add(1);
			
				match op {
					OP_CMPS_IMMEDIATE => immediate24!(5, Self::instr_cmps),
					OP_CMPS_DIRECT => direct!(7, Self::instr_cmps),
					OP_CMPS_INDEXED => indexed!(7, Self::instr_cmps),
					OP_CMPS_EXTENDED => extended!(8, Self::instr_cmps),
					OP_CMPU_IMMEDIATE => immediate24!(5, Self::instr_cmpu),
					OP_CMPU_DIRECT => direct!(7, Self::instr_cmpu),
					OP_CMPU_INDEXED => indexed!(7, Self::instr_cmpu),
					OP_CMPU_EXTENDED => extended!(8, Self::instr_cmpu),
					OP_SWI3 => inherent!(20, Self::instr_swi3),
					_ => invalid_opcode!(op)
				}
			},
			OP_ABX => inherent!(3, Self::instr_abx),
			OP_ADCA_IMMEDIATE => immediate8!(2, Self::instr_adca),
			OP_ADCA_DIRECT => direct!(4, Self::instr_adca),
			OP_ADCA_INDEXED => indexed!(4, Self::instr_adca),
			OP_ADCA_EXTENDED => extended!(5, Self::instr_adca),
			OP_ADCB_IMMEDIATE => immediate8!(2, Self::instr_adcb),
			OP_ADCB_DIRECT => direct!(4, Self::instr_adcb),
			OP_ADCB_INDEXED => indexed!(4, Self::instr_adcb),
			OP_ADCB_EXTENDED => extended!(5, Self::instr_adcb),
			OP_ADDA_IMMEDIATE => immediate8!(2, Self::instr_adda),
			OP_ADDA_DIRECT => direct!(4, Self::instr_adda),
			OP_ADDA_INDEXED => indexed!(4, Self::instr_adda),
			OP_ADDA_EXTENDED => extended!(5, Self::instr_adda),
			OP_ADDB_IMMEDIATE => immediate8!(2, Self::instr_addb),
			OP_ADDB_DIRECT => direct!(4, Self::instr_addb),
			OP_ADDB_INDEXED => indexed!(4, Self::instr_addb),
			OP_ADDB_EXTENDED => extended!(5, Self::instr_addb),
			OP_ADDD_IMMEDIATE => immediate16!(4, Self::instr_addd),
			OP_ADDD_DIRECT => direct!(6, Self::instr_addd),
			OP_ADDD_INDEXED => indexed!(6, Self::instr_addd),
			OP_ADDD_EXTENDED => extended!(7, Self::instr_addd),
			OP_ANDA_IMMEDIATE => immediate8!(2, Self::instr_anda),
			OP_ANDA_DIRECT => direct!(4, Self::instr_anda),
			OP_ANDA_INDEXED => indexed!(4, Self::instr_anda),
			OP_ANDA_EXTENDED => extended!(5, Self::instr_anda),
			OP_ANDB_IMMEDIATE => immediate8!(2, Self::instr_andb),
			OP_ANDB_DIRECT => direct!(4, Self::instr_andb),
			OP_ANDB_INDEXED => indexed!(4, Self::instr_andb),
			OP_ANDB_EXTENDED => extended!(5, Self::instr_andb),
			OP_ANDCC_IMMEDIATE => immediate8!(3, Self::instr_andcc),
			OP_ASLA => inherent!(2, Self::instr_asla),
			OP_ASLB => inherent!(2, Self::instr_aslb),
			OP_ASL_DIRECT => direct!(6, Self::instr_asl),
			OP_ASL_INDEXED => indexed!(6, Self::instr_asl),
			OP_ASL_EXTENDED => extended!(7, Self::instr_asl),
			OP_ASRA => inherent!(2, Self::instr_asra),
			OP_ASRB => inherent!(2, Self::instr_asrb),
			OP_ASR_DIRECT => direct!(6, Self::instr_asr),
			OP_ASR_INDEXED => indexed!(6, Self::instr_asr),
			OP_ASR_EXTENDED => extended!(7, Self::instr_asr),
			OP_BITA_IMMEDIATE => immediate8!(2, Self::instr_bita),
			OP_BITA_DIRECT => direct!(4, Self::instr_bita),
			OP_BITA_INDEXED => indexed!(4, Self::instr_bita),
			OP_BITA_EXTENDED => extended!(5, Self::instr_bita),
			OP_BITB_IMMEDIATE => immediate8!(2, Self::instr_bitb),
			OP_BITB_DIRECT => direct!(4, Self::instr_bitb),
			OP_BITB_INDEXED => indexed!(4, Self::instr_bitb),
			OP_BITB_EXTENDED => extended!(5, Self::instr_bitb),
			OP_CLRA => inherent!(2, Self::instr_clra),
			OP_CLRB => inherent!(2, Self::instr_clrb),
			OP_CLR_DIRECT => direct!(6, Self::instr_clr),
			OP_CLR_INDEXED => indexed!(6, Self::instr_clr),
			OP_CLR_EXTENDED => extended!(7, Self::instr_clr),
			OP_CMPA_IMMEDIATE => immediate8!(2, Self::instr_cmpa),
			OP_CMPA_DIRECT => direct!(4, Self::instr_cmpa),
			OP_CMPA_INDEXED => indexed!(4, Self::instr_cmpa),
			OP_CMPA_EXTENDED => extended!(5, Self::instr_cmpa),
			OP_CMPB_IMMEDIATE => immediate8!(2, Self::instr_cmpb),
			OP_CMPB_DIRECT => direct!(4, Self::instr_cmpb),
			OP_CMPB_INDEXED => indexed!(4, Self::instr_cmpb),
			OP_CMPB_EXTENDED => extended!(5, Self::instr_cmpb),
			OP_CMPX_IMMEDIATE => immediate16!(4, Self::instr_cmpx),
			OP_CMPX_DIRECT => direct!(6, Self::instr_cmpx),
			OP_CMPX_INDEXED => indexed!(6, Self::instr_cmpx),
			OP_CMPX_EXTENDED => extended!(7, Self::instr_cmpx),
			OP_LSRA => inherent!(2, Self::instr_lsra),
			OP_LSRB => inherent!(2, Self::instr_lsrb),
			OP_LSR_DIRECT => direct!(6, Self::instr_lsr),
			OP_LSR_INDEXED => indexed!(6, Self::instr_lsr),
			OP_LSR_EXTENDED => extended!(7, Self::instr_lsr),
			OP_MUL => inherent!(11, Self::instr_mul),
			OP_NEGA => inherent!(2, Self::instr_nega),
			OP_NEGB => inherent!(2, Self::instr_negb),
			OP_NEG_DIRECT => direct!(6, Self::instr_neg),
			OP_NEG_INDEXED => indexed!(6, Self::instr_neg),
			OP_NEG_EXTENDED => extended!(7, Self::instr_neg),
			OP_NOP => inherent!(2, Self::instr_nop),
			OP_ORA_IMMEDIATE => immediate8!(2, Self::instr_ora),
			OP_ORA_DIRECT => direct!(4, Self::instr_ora),
			OP_ORA_INDEXED => indexed!(4, Self::instr_ora),
			OP_ORA_EXTENDED => extended!(5, Self::instr_ora),
			OP_ORB_IMMEDIATE => immediate8!(2, Self::instr_orb),
			OP_ORB_DIRECT => direct!(4, Self::instr_orb),
			OP_ORB_INDEXED => indexed!(4, Self::instr_orb),
			OP_ORB_EXTENDED => extended!(5, Self::instr_orb),
			OP_ORCC_IMMEDIATE => immediate8!(3, Self::instr_orcc),
			OP_PSHS_IMMEDIATE => immediate8!(5, Self::instr_pshs),
			OP_PSHU_IMMEDIATE => immediate8!(5, Self::instr_pshu),
			OP_PULS_IMMEDIATE => immediate8!(5, Self::instr_puls),
			OP_PULU_IMMEDIATE => immediate8!(5, Self::instr_pulu),
			OP_ROLA => inherent!(2, Self::instr_rola),
			OP_ROLB => inherent!(2, Self::instr_rolb),
			OP_ROL_DIRECT => direct!(6, Self::instr_rol),
			OP_ROL_INDEXED => indexed!(6, Self::instr_rol),
			OP_ROL_EXTENDED => extended!(7, Self::instr_rol),
			OP_RORA => inherent!(2, Self::instr_rora),
			OP_RORB => inherent!(2, Self::instr_rorb),
			OP_ROR_DIRECT => direct!(6, Self::instr_ror),
			OP_ROR_INDEXED => indexed!(6, Self::instr_ror),
			OP_ROR_EXTENDED => extended!(7, Self::instr_ror),
			OP_RTI => inherent!(6, Self::instr_rti),
			OP_RTS => inherent!(5, Self::instr_rts),
			OP_SBCA_IMMEDIATE => immediate8!(2, Self::instr_sbca),
			OP_SBCA_DIRECT => direct!(4, Self::instr_sbca),
			OP_SBCA_INDEXED => indexed!(4, Self::instr_sbca),
			OP_SBCA_EXTENDED => extended!(5, Self::instr_sbca),
			OP_SBCB_IMMEDIATE => immediate8!(2, Self::instr_sbcb),
			OP_SBCB_DIRECT => direct!(4, Self::instr_sbcb),
			OP_SBCB_INDEXED => indexed!(4, Self::instr_sbcb),
			OP_SBCB_EXTENDED => extended!(5, Self::instr_sbcb),
			OP_SEX => inherent!(2, Self::instr_sex),
			OP_COMA => inherent!(2, Self::instr_coma),
			OP_COMB => inherent!(2, Self::instr_comb),
			OP_COM_DIRECT => direct!(6, Self::instr_com),
			OP_COM_INDEXED => indexed!(6, Self::instr_com),
			OP_COM_EXTENDED => extended!(7, Self::instr_com),
			OP_CWAI_IMMEDIATE => immediate8!(22, Self::instr_cwai),
			OP_DAA => inherent!(2, Self::instr_daa),
			OP_DECA => inherent!(2, Self::instr_deca),
			OP_DECB => inherent!(2, Self::instr_decb),
			OP_DEC_DIRECT => direct!(6, Self::instr_dec),
			OP_DEC_INDEXED => indexed!(6, Self::instr_dec),
			OP_DEC_EXTENDED => extended!(7, Self::instr_dec),
			OP_EORA_IMMEDIATE => immediate8!(2, Self::instr_eora),
			OP_EORA_DIRECT => direct!(4, Self::instr_eora),
			OP_EORA_INDEXED => indexed!(4, Self::instr_eora),
			OP_EORA_EXTENDED => extended!(5, Self::instr_eora),
			OP_EORB_IMMEDIATE => immediate8!(2, Self::instr_eorb),
			OP_EORB_DIRECT => direct!(4, Self::instr_eorb),
			OP_EORB_INDEXED => indexed!(4, Self::instr_eorb),
			OP_EORB_EXTENDED => extended!(5, Self::instr_eorb),
			OP_EXG_IMMEDIATE => immediate8!(8, Self::instr_exg),
			OP_INCA => inherent!(2, Self::instr_inca),
			OP_INCB => inherent!(2, Self::instr_incb),
			OP_INC_DIRECT => direct!(6, Self::instr_inc),
			OP_INC_INDEXED => indexed!(6, Self::instr_inc),
			OP_INC_EXTENDED => extended!(7, Self::instr_inc),
			OP_JMP_DIRECT => direct!(3, Self::instr_jmp),
			OP_JMP_INDEXED => indexed!(3, Self::instr_jmp),
			OP_JMP_EXTENDED => extended!(4, Self::instr_jmp),
			OP_JSR_DIRECT => direct!(7, Self::instr_jsr),
			OP_JSR_INDEXED => indexed!(7, Self::instr_jsr),
			OP_JSR_EXTENDED => extended!(8, Self::instr_jsr),
			OP_LDA_IMMEDIATE => immediate8!(2, Self::instr_lda),
			OP_LDA_DIRECT => direct!(4, Self::instr_lda),
			OP_LDA_INDEXED => indexed!(4, Self::instr_lda),
			OP_LDA_EXTENDED => extended!(5, Self::instr_lda),
			OP_LDB_IMMEDIATE => immediate8!(2, Self::instr_ldb),
			OP_LDB_DIRECT => direct!(4, Self::instr_ldb),
			OP_LDB_INDEXED => indexed!(4, Self::instr_ldb),
			OP_LDB_EXTENDED => extended!(5, Self::instr_ldb),
			OP_LDD_IMMEDIATE => immediate16!(3, Self::instr_ldd),
			OP_LDD_DIRECT => direct!(5, Self::instr_ldd),
			OP_LDD_INDEXED => indexed!(5, Self::instr_ldd),
			OP_LDD_EXTENDED => extended!(6, Self::instr_ldd),
			OP_LDU_IMMEDIATE => immediate16!(3, Self::instr_ldu),
			OP_LDU_DIRECT => direct!(5, Self::instr_ldu),
			OP_LDU_INDEXED => indexed!(5, Self::instr_ldu),
			OP_LDU_EXTENDED => extended!(6, Self::instr_ldu),
			OP_LDX_IMMEDIATE => immediate16!(3, Self::instr_ldx),
			OP_LDX_DIRECT => direct!(5, Self::instr_ldx),
			OP_LDX_INDEXED => indexed!(5, Self::instr_ldx),
			OP_LDX_EXTENDED => extended!(6, Self::instr_ldx),
			OP_LEAS_INDEXED => indexed!(4, Self::instr_leas),
			OP_LEAU_INDEXED => indexed!(4, Self::instr_leau),
			OP_LEAX_INDEXED => indexed!(4, Self::instr_leax),
			OP_LEAY_INDEXED => indexed!(4, Self::instr_leay),
			OP_STA_DIRECT => direct!(4, Self::instr_sta),
			OP_STA_INDEXED => indexed!(4, Self::instr_sta),
			OP_STA_EXTENDED => extended!(5, Self::instr_sta),
			OP_STB_DIRECT => direct!(4, Self::instr_stb),
			OP_STB_INDEXED => indexed!(4, Self::instr_stb),
			OP_STB_EXTENDED => extended!(5, Self::instr_stb),
			OP_STD_DIRECT => direct!(5, Self::instr_std),
			OP_STD_INDEXED => indexed!(5, Self::instr_std),
			OP_STD_EXTENDED => extended!(6, Self::instr_std),
			OP_STU_DIRECT => direct!(5, Self::instr_stu),
			OP_STU_INDEXED => indexed!(5, Self::instr_stu),
			OP_STU_EXTENDED => extended!(6, Self::instr_stu),
			OP_STX_DIRECT => direct!(5, Self::instr_stx),
			OP_STX_INDEXED => indexed!(5, Self::instr_stx),
			OP_STX_EXTENDED => extended!(6, Self::instr_stx),
			OP_SUBA_IMMEDIATE => immediate8!(2, Self::instr_suba),
			OP_SUBA_DIRECT => direct!(4, Self::instr_suba),
			OP_SUBA_INDEXED => indexed!(4, Self::instr_suba),
			OP_SUBA_EXTENDED => extended!(5, Self::instr_suba),
			OP_SUBB_IMMEDIATE => immediate8!(2, Self::instr_subb),
			OP_SUBB_DIRECT => direct!(4, Self::instr_subb),
			OP_SUBB_INDEXED => indexed!(4, Self::instr_subb),
			OP_SUBB_EXTENDED => extended!(5, Self::instr_subb),
			OP_SUBD_IMMEDIATE => immediate16!(4, Self::instr_subd),
			OP_SUBD_DIRECT => direct!(6, Self::instr_subd),
			OP_SUBD_INDEXED => indexed!(6, Self::instr_subd),
			OP_SUBD_EXTENDED => extended!(7, Self::instr_subd),
			OP_SWI => inherent!(19, Self::instr_swi),
			OP_SYNC => inherent!(2, Self::instr_sync),
			OP_TFR1_IMMEDIATE => immediate8!(6, Self::instr_tfr1),
			OP_TSTA => inherent!(2, Self::instr_tsta),
			OP_TSTB => inherent!(2, Self::instr_tstb),
			OP_TST_DIRECT => direct!(6, Self::instr_tst),
			OP_TST_INDEXED => indexed!(6, Self::instr_tst),
			OP_TST_EXTENDED => extended!(7, Self::instr_tst),
			_ => invalid_opcode!(op)
		}

		self.reg_pc = next_pc;

		cycles
	}

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
		panic!("Unimplemented instruction CMPA");
	}

	fn instr_cmpb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CMPB");
	}

	fn instr_cmpd(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CMPD");
	}

	fn instr_cmps(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CMPS");
	}

	fn instr_cmpu(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CMPU");
	}

	fn instr_cmpx(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CMPX");
	}

	fn instr_cmpy(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction CMPY");
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

	fn instr_jsr(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction JSR");
	}

	fn instr_lda(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LDA");
	}

	fn instr_ldb(&mut self, mobo: &Motherboard, addr: u16) {
		panic!("Unimplemented instruction LDB");
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

	fn sub_u8_and_set_flags(&self, a: u8, b: u8) {
		let result = b - a;
	}

	fn sub_u16_and_set_flags(&self, a: u16, b: u16) {
		let result = b - a;
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