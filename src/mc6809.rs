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
		    ($f:path, $cycles:expr) => ({
		    	cycles += cycles;
		    	$f(self, motherboard);
		    })
		}

		macro_rules! immediate8 {
			($f:path, $cycles:expr) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(1);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! immediate16 {
			($f:path, $cycles:expr) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(2);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! immediate24 {
			($f:path, $cycles:expr) => ({
				let addr = next_pc;
				next_pc = next_pc.wrapping_add(3);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! direct {
			($f:path, $cycles:expr) => ({
				let mut addr = motherboard.read_u8(next_pc) as u16;
				addr = addr as u16 | (self.reg_dp as u16) << 8;
				next_pc = next_pc.wrapping_add(1);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! extended {
			($f:expr, $cycles:expr) => ({
				let addr = motherboard.read_u16(next_pc);
				next_pc = next_pc.wrapping_add(2);
				cycles += $cycles;
				$f(self, motherboard, addr)
			})
		}

		macro_rules! indexed {
			($f: expr, $cycles:expr) => ({
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
					OP_CMPD_IMMEDIATE => immediate24!(Self::instr_cmpd, 5),
					OP_CMPD_DIRECT => direct!(Self::instr_cmpd, 7),
					OP_CMPD_INDEXED => indexed!(Self::instr_cmpd, 7),
					OP_CMPD_EXTENDED => extended!(Self::instr_cmpd, 8),
					OP_CMPY_IMMEDIATE => immediate24!(Self::instr_cmpy, 5),
					OP_CMPY_DIRECT => direct!(Self::instr_cmpy, 7),
					OP_CMPY_INDEXED => indexed!(Self::instr_cmpy, 7),
					OP_CMPY_EXTENDED => extended!(Self::instr_cmpy, 8),
					OP_LDS_IMMEDIATE => immediate24!(Self::instr_lds, 4),
					OP_LDS_DIRECT => direct!(Self::instr_lds, 6),
					OP_LDS_INDEXED => indexed!(Self::instr_lds, 6),
					OP_LDS_EXTENDED => extended!(Self::instr_lds, 7),
					OP_LDY_IMMEDIATE => immediate24!(Self::instr_ldy, 4),
					OP_LDY_DIRECT => direct!(Self::instr_ldy, 6),
					OP_LDY_INDEXED => indexed!(Self::instr_ldy, 6),
					OP_LDY_EXTENDED => extended!(Self::instr_ldy, 7),
					OP_STY_DIRECT => direct!(Self::instr_sty, 6),
					OP_STY_INDEXED => indexed!(Self::instr_sty, 6),
					OP_STY_EXTENDED => extended!(Self::instr_sty, 7),
					OP_SWI2 => inherent!(Self::instr_swi2, 20),
					_ => invalid_opcode!(op)
				}
			},
			OP_PAGE_3 => {
				let op = motherboard.read_u8(next_pc);
				next_pc = next_pc.wrapping_add(1);
			
				match op {
					OP_CMPS_IMMEDIATE => immediate24!(Self::instr_cmps, 5),
					OP_CMPS_DIRECT => direct!(Self::instr_cmps, 7),
					OP_CMPS_INDEXED => indexed!(Self::instr_cmps, 7),
					OP_CMPS_EXTENDED => extended!(Self::instr_cmps, 8),
					OP_CMPU_IMMEDIATE => immediate24!(Self::instr_cmpu, 5),
					OP_CMPU_DIRECT => direct!(Self::instr_cmpu, 7),
					OP_CMPU_INDEXED => indexed!(Self::instr_cmpu, 7),
					OP_CMPU_EXTENDED => extended!(Self::instr_cmpu, 8),
					OP_SWI3 => inherent!(Self::instr_swi3, 20),
					_ => invalid_opcode!(op)
				}
			},
			OP_ABX => inherent!(Self::instr_abx, 3),
			OP_ADCA_IMMEDIATE => immediate8!(Self::instr_adca, 2),
			OP_ADCA_DIRECT => direct!(Self::instr_adca, 4),
			OP_ADCA_INDEXED => indexed!(Self::instr_adca, 4),
			OP_ADCA_EXTENDED => extended!(Self::instr_adca, 5),
			OP_ADCB_IMMEDIATE => immediate8!(Self::instr_adcb, 2),
			OP_ADCB_DIRECT => direct!(Self::instr_adcb, 4),
			OP_ADCB_INDEXED => indexed!(Self::instr_adcb, 4),
			OP_ADCB_EXTENDED => extended!(Self::instr_adcb, 5),
			OP_ADDA_IMMEDIATE => immediate8!(Self::instr_adda, 2),
			OP_ADDA_DIRECT => direct!(Self::instr_adda, 4),
			OP_ADDA_INDEXED => indexed!(Self::instr_adda, 4),
			OP_ADDA_EXTENDED => extended!(Self::instr_adda, 5),
			OP_ADDB_IMMEDIATE => immediate8!(Self::instr_addb, 2),
			OP_ADDB_DIRECT => direct!(Self::instr_addb, 4),
			OP_ADDB_INDEXED => indexed!(Self::instr_addb, 4),
			OP_ADDB_EXTENDED => extended!(Self::instr_addb, 5),
			OP_ADDD_IMMEDIATE => immediate16!(Self::instr_addd, 4),
			OP_ADDD_DIRECT => direct!(Self::instr_addd, 6),
			OP_ADDD_INDEXED => indexed!(Self::instr_addd, 6),
			OP_ADDD_EXTENDED => extended!(Self::instr_addd, 7),
			OP_ANDA_IMMEDIATE => immediate8!(Self::instr_anda, 2),
			OP_ANDA_DIRECT => direct!(Self::instr_anda, 4),
			OP_ANDA_INDEXED => indexed!(Self::instr_anda, 4),
			OP_ANDA_EXTENDED => extended!(Self::instr_anda, 5),
			OP_ANDB_IMMEDIATE => immediate8!(Self::instr_andb, 2),
			OP_ANDB_DIRECT => direct!(Self::instr_andb, 4),
			OP_ANDB_INDEXED => indexed!(Self::instr_andb, 4),
			OP_ANDB_EXTENDED => extended!(Self::instr_andb, 5),
			OP_ANDCC_IMMEDIATE => immediate8!(Self::instr_andcc, 3),
			OP_ASLA => inherent!(Self::instr_asla, 2),
			OP_ASLB => inherent!(Self::instr_aslb, 2),
			OP_ASL_DIRECT => direct!(Self::instr_asl, 6),
			OP_ASL_INDEXED => indexed!(Self::instr_asl, 6),
			OP_ASL_EXTENDED => extended!(Self::instr_asl, 7),
			OP_ASRA => inherent!(Self::instr_asra, 2),
			OP_ASRB => inherent!(Self::instr_asrb, 2),
			OP_ASR_DIRECT => direct!(Self::instr_asr, 6),
			OP_ASR_INDEXED => indexed!(Self::instr_asr, 6),
			OP_ASR_EXTENDED => extended!(Self::instr_asr, 7),
			OP_BITA_IMMEDIATE => immediate8!(Self::instr_bita, 2),
			OP_BITA_DIRECT => direct!(Self::instr_bita, 4),
			OP_BITA_INDEXED => indexed!(Self::instr_bita, 4),
			OP_BITA_EXTENDED => extended!(Self::instr_bita, 5),
			OP_BITB_IMMEDIATE => immediate8!(Self::instr_bitb, 2),
			OP_BITB_DIRECT => direct!(Self::instr_bitb, 4),
			OP_BITB_INDEXED => indexed!(Self::instr_bitb, 4),
			OP_BITB_EXTENDED => extended!(Self::instr_bitb, 5),
			OP_CLRA => inherent!(Self::instr_clra, 2),
			OP_CLRB => inherent!(Self::instr_clrb, 2),
			OP_CLR_DIRECT => direct!(Self::instr_clr, 6),
			OP_CLR_INDEXED => indexed!(Self::instr_clr, 6),
			OP_CLR_EXTENDED => extended!(Self::instr_clr, 7),
			OP_CMPA_IMMEDIATE => immediate8!(Self::instr_cmpa, 2),
			OP_CMPA_DIRECT => direct!(Self::instr_cmpa, 4),
			OP_CMPA_INDEXED => indexed!(Self::instr_cmpa, 4),
			OP_CMPA_EXTENDED => extended!(Self::instr_cmpa, 5),
			OP_CMPB_IMMEDIATE => immediate8!(Self::instr_cmpb, 2),
			OP_CMPB_DIRECT => direct!(Self::instr_cmpb, 4),
			OP_CMPB_INDEXED => indexed!(Self::instr_cmpb, 4),
			OP_CMPB_EXTENDED => extended!(Self::instr_cmpb, 5),
			OP_CMPX_IMMEDIATE => immediate16!(Self::instr_cmpx, 4),
			OP_CMPX_DIRECT => direct!(Self::instr_cmpx, 6),
			OP_CMPX_INDEXED => indexed!(Self::instr_cmpx, 6),
			OP_CMPX_EXTENDED => extended!(Self::instr_cmpx, 7),
			OP_LSRA => inherent!(Self::instr_lsra, 2),
			OP_LSRB => inherent!(Self::instr_lsrb, 2),
			OP_LSR_DIRECT => direct!(Self::instr_lsr, 6),
			OP_LSR_INDEXED => indexed!(Self::instr_lsr, 6),
			OP_LSR_EXTENDED => extended!(Self::instr_lsr, 7),
			OP_MUL => inherent!(Self::instr_mul, 11),
			OP_NEGA => inherent!(Self::instr_nega, 2),
			OP_NEGB => inherent!(Self::instr_negb, 2),
			OP_NEG_DIRECT => direct!(Self::instr_neg, 6),
			OP_NEG_INDEXED => indexed!(Self::instr_neg, 6),
			OP_NEG_EXTENDED => extended!(Self::instr_neg, 7),
			OP_NOP => inherent!(Self::instr_nop, 2),
			OP_ORA_IMMEDIATE => immediate8!(Self::instr_ora, 2),
			OP_ORA_DIRECT => direct!(Self::instr_ora, 4),
			OP_ORA_INDEXED => indexed!(Self::instr_ora, 4),
			OP_ORA_EXTENDED => extended!(Self::instr_ora, 5),
			OP_ORB_IMMEDIATE => immediate8!(Self::instr_orb, 2),
			OP_ORB_DIRECT => direct!(Self::instr_orb, 4),
			OP_ORB_INDEXED => indexed!(Self::instr_orb, 4),
			OP_ORB_EXTENDED => extended!(Self::instr_orb, 5),
			OP_ORCC_IMMEDIATE => immediate8!(Self::instr_orcc, 3),
			OP_PSHS_IMMEDIATE => immediate8!(Self::instr_pshs, 5),
			OP_PSHU_IMMEDIATE => immediate8!(Self::instr_pshu, 5),
			OP_PULS_IMMEDIATE => immediate8!(Self::instr_puls, 5),
			OP_PULU_IMMEDIATE => immediate8!(Self::instr_pulu, 5),
			OP_ROLA => inherent!(Self::instr_rola, 2),
			OP_ROLB => inherent!(Self::instr_rolb, 2),
			OP_ROL_DIRECT => direct!(Self::instr_rol, 6),
			OP_ROL_INDEXED => indexed!(Self::instr_rol, 6),
			OP_ROL_EXTENDED => extended!(Self::instr_rol, 7),
			OP_RORA => inherent!(Self::instr_rora, 2),
			OP_RORB => inherent!(Self::instr_rorb, 2),
			OP_ROR_DIRECT => direct!(Self::instr_ror, 6),
			OP_ROR_INDEXED => indexed!(Self::instr_ror, 6),
			OP_ROR_EXTENDED => extended!(Self::instr_ror, 7),
			OP_RTI => inherent!(Self::instr_rti, 6),
			OP_RTS => inherent!(Self::instr_rts, 5),
			OP_SBCA_IMMEDIATE => immediate8!(Self::instr_sbca, 2),
			OP_SBCA_DIRECT => direct!(Self::instr_sbca, 4),
			OP_SBCA_INDEXED => indexed!(Self::instr_sbca, 4),
			OP_SBCA_EXTENDED => extended!(Self::instr_sbca, 5),
			OP_SBCB_IMMEDIATE => immediate8!(Self::instr_sbcb, 2),
			OP_SBCB_DIRECT => direct!(Self::instr_sbcb, 4),
			OP_SBCB_INDEXED => indexed!(Self::instr_sbcb, 4),
			OP_SBCB_EXTENDED => extended!(Self::instr_sbcb, 5),
			OP_SEX => inherent!(Self::instr_sex, 2),
			OP_COMA => inherent!(Self::instr_coma, 2),
			OP_COMB => inherent!(Self::instr_comb, 2),
			OP_COM_DIRECT => direct!(Self::instr_com, 6),
			OP_COM_INDEXED => indexed!(Self::instr_com, 6),
			OP_COM_EXTENDED => extended!(Self::instr_com, 7),
			OP_CWAI_IMMEDIATE => immediate8!(Self::instr_cwai, 22),
			OP_DAA => inherent!(Self::instr_daa, 2),
			OP_DECA => inherent!(Self::instr_deca, 2),
			OP_DECB => inherent!(Self::instr_decb, 2),
			OP_DEC_DIRECT => direct!(Self::instr_dec, 6),
			OP_DEC_INDEXED => indexed!(Self::instr_dec, 6),
			OP_DEC_EXTENDED => extended!(Self::instr_dec, 7),
			OP_EORA_IMMEDIATE => immediate8!(Self::instr_eora, 2),
			OP_EORA_DIRECT => direct!(Self::instr_eora, 4),
			OP_EORA_INDEXED => indexed!(Self::instr_eora, 4),
			OP_EORA_EXTENDED => extended!(Self::instr_eora, 5),
			OP_EORB_IMMEDIATE => immediate8!(Self::instr_eorb, 2),
			OP_EORB_DIRECT => direct!(Self::instr_eorb, 4),
			OP_EORB_INDEXED => indexed!(Self::instr_eorb, 4),
			OP_EORB_EXTENDED => extended!(Self::instr_eorb, 5),
			OP_EXG_IMMEDIATE => immediate8!(Self::instr_exg, 8),
			OP_INCA => inherent!(Self::instr_inca, 2),
			OP_INCB => inherent!(Self::instr_incb, 2),
			OP_INC_DIRECT => direct!(Self::instr_inc, 6),
			OP_INC_INDEXED => indexed!(Self::instr_inc, 6),
			OP_INC_EXTENDED => extended!(Self::instr_inc, 7),
			OP_JMP_DIRECT => direct!(Self::instr_jmp, 3),
			OP_JMP_INDEXED => indexed!(Self::instr_jmp, 3),
			OP_JMP_EXTENDED => extended!(Self::instr_jmp, 4),
			OP_JSR_DIRECT => direct!(Self::instr_jsr, 7),
			OP_JSR_INDEXED => indexed!(Self::instr_jsr, 7),
			OP_JSR_EXTENDED => extended!(Self::instr_jsr, 8),
			OP_LDA_IMMEDIATE => immediate8!(Self::instr_lda, 2),
			OP_LDA_DIRECT => direct!(Self::instr_lda, 4),
			OP_LDA_INDEXED => indexed!(Self::instr_lda, 4),
			OP_LDA_EXTENDED => extended!(Self::instr_lda, 5),
			OP_LDB_IMMEDIATE => immediate8!(Self::instr_ldb, 2),
			OP_LDB_DIRECT => direct!(Self::instr_ldb, 4),
			OP_LDB_INDEXED => indexed!(Self::instr_ldb, 4),
			OP_LDB_EXTENDED => extended!(Self::instr_ldb, 5),
			OP_LDD_IMMEDIATE => immediate16!(Self::instr_ldd, 3),
			OP_LDD_DIRECT => direct!(Self::instr_ldd, 5),
			OP_LDD_INDEXED => indexed!(Self::instr_ldd, 5),
			OP_LDD_EXTENDED => extended!(Self::instr_ldd, 6),
			OP_LDU_IMMEDIATE => immediate16!(Self::instr_ldu, 3),
			OP_LDU_DIRECT => direct!(Self::instr_ldu, 5),
			OP_LDU_INDEXED => indexed!(Self::instr_ldu, 5),
			OP_LDU_EXTENDED => extended!(Self::instr_ldu, 6),
			OP_LDX_IMMEDIATE => immediate16!(Self::instr_ldx, 3),
			OP_LDX_DIRECT => direct!(Self::instr_ldx, 5),
			OP_LDX_INDEXED => indexed!(Self::instr_ldx, 5),
			OP_LDX_EXTENDED => extended!(Self::instr_ldx, 6),
			OP_LEAS_INDEXED => indexed!(Self::instr_leas, 4),
			OP_LEAU_INDEXED => indexed!(Self::instr_leau, 4),
			OP_LEAX_INDEXED => indexed!(Self::instr_leax, 4),
			OP_LEAY_INDEXED => indexed!(Self::instr_leay, 4),
			OP_STA_DIRECT => direct!(Self::instr_sta, 4),
			OP_STA_INDEXED => indexed!(Self::instr_sta, 4),
			OP_STA_EXTENDED => extended!(Self::instr_sta, 5),
			OP_STB_DIRECT => direct!(Self::instr_stb, 4),
			OP_STB_INDEXED => indexed!(Self::instr_stb, 4),
			OP_STB_EXTENDED => extended!(Self::instr_stb, 5),
			OP_STD_DIRECT => direct!(Self::instr_std, 5),
			OP_STD_INDEXED => indexed!(Self::instr_std, 5),
			OP_STD_EXTENDED => extended!(Self::instr_std, 6),
			OP_STU_DIRECT => direct!(Self::instr_stu, 5),
			OP_STU_INDEXED => indexed!(Self::instr_stu, 5),
			OP_STU_EXTENDED => extended!(Self::instr_stu, 6),
			OP_STX_DIRECT => direct!(Self::instr_stx, 5),
			OP_STX_INDEXED => indexed!(Self::instr_stx, 5),
			OP_STX_EXTENDED => extended!(Self::instr_stx, 6),
			OP_SUBA_IMMEDIATE => immediate8!(Self::instr_suba, 2),
			OP_SUBA_DIRECT => direct!(Self::instr_suba, 4),
			OP_SUBA_INDEXED => indexed!(Self::instr_suba, 4),
			OP_SUBA_EXTENDED => extended!(Self::instr_suba, 5),
			OP_SUBB_IMMEDIATE => immediate8!(Self::instr_subb, 2),
			OP_SUBB_DIRECT => direct!(Self::instr_subb, 4),
			OP_SUBB_INDEXED => indexed!(Self::instr_subb, 4),
			OP_SUBB_EXTENDED => extended!(Self::instr_subb, 5),
			OP_SUBD_IMMEDIATE => immediate16!(Self::instr_subd, 4),
			OP_SUBD_DIRECT => direct!(Self::instr_subd, 6),
			OP_SUBD_INDEXED => indexed!(Self::instr_subd, 6),
			OP_SUBD_EXTENDED => extended!(Self::instr_subd, 7),
			OP_SWI => inherent!(Self::instr_swi, 19),
			OP_SYNC => inherent!(Self::instr_sync, 2),
			OP_TFR1_IMMEDIATE => immediate8!(Self::instr_tfr1, 6),
			OP_TSTA => inherent!(Self::instr_tsta, 2),
			OP_TSTB => inherent!(Self::instr_tstb, 2),
			OP_TST_DIRECT => direct!(Self::instr_tst, 6),
			OP_TST_INDEXED => indexed!(Self::instr_tst, 6),
			OP_TST_EXTENDED => extended!(Self::instr_tst, 7),
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