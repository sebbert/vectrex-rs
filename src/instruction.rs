#![allow(dead_code)]

pub const OP_PAGE_2: u8 = 0x10;
pub const OP_PAGE_3: u8 = 0x11;

pub const OP_ABX: u8 = 0x3a;

pub const OP_ADCA_IMMEDIATE: u8 = 0x89;
pub const OP_ADCA_DIRECT: u8 = 0x99;
pub const OP_ADCA_INDEXED: u8 = 0xa9;
pub const OP_ADCA_EXTENDED: u8 = 0xb9;

pub const OP_ADCB_IMMEDIATE: u8 = 0xc9;
pub const OP_ADCB_DIRECT: u8 = 0xd9;
pub const OP_ADCB_INDEXED: u8 = 0xe9;
pub const OP_ADCB_EXTENDED: u8 = 0xf9;

pub const OP_ADDA_IMMEDIATE: u8 = 0x8b;
pub const OP_ADDA_DIRECT: u8 = 0x9b;
pub const OP_ADDA_INDEXED: u8 = 0xab;
pub const OP_ADDA_EXTENDED: u8 = 0xbb;

pub const OP_ADDB_IMMEDIATE: u8 = 0xcb;
pub const OP_ADDB_DIRECT: u8 = 0xdb;
pub const OP_ADDB_INDEXED: u8 = 0xeb;
pub const OP_ADDB_EXTENDED: u8 = 0xfb;

pub const OP_ADDD_IMMEDIATE: u8 = 0xc3;
pub const OP_ADDD_DIRECT: u8 = 0xd3;
pub const OP_ADDD_INDEXED: u8 = 0xe3;
pub const OP_ADDD_EXTENDED: u8 = 0xf3;

pub const OP_ANDA_IMMEDIATE: u8 = 0x84;
pub const OP_ANDA_DIRECT: u8 = 0x94;
pub const OP_ANDA_INDEXED: u8 = 0xa4;
pub const OP_ANDA_EXTENDED: u8 = 0xb4;

pub const OP_ANDB_IMMEDIATE: u8 = 0xc4;
pub const OP_ANDB_DIRECT: u8 = 0xd4;
pub const OP_ANDB_INDEXED: u8 = 0xe4;
pub const OP_ANDB_EXTENDED: u8 = 0xf4;

pub const OP_ANDCC_IMMEDIATE: u8 = 0x1c;

pub const OP_ASLA: u8 = 0x48;

pub const OP_ASLB: u8 = 0x58;

pub const OP_ASL_DIRECT: u8 = 0x08;
pub const OP_ASL_INDEXED: u8 = 0x68;
pub const OP_ASL_EXTENDED: u8 = 0x78;

pub const OP_ASRA: u8 = 0x47;

pub const OP_ASRB: u8 = 0x57;

pub const OP_ASR_DIRECT: u8 = 0x07;
pub const OP_ASR_INDEXED: u8 = 0x67;
pub const OP_ASR_EXTENDED: u8 = 0x77;

pub const OP_BITA_IMMEDIATE: u8 = 0x85;
pub const OP_BITA_DIRECT: u8 = 0x95;
pub const OP_BITA_INDEXED: u8 = 0xa5;
pub const OP_BITA_EXTENDED: u8 = 0xb5;

pub const OP_BITB_IMMEDIATE: u8 = 0xc5;
pub const OP_BITB_DIRECT: u8 = 0xd5;
pub const OP_BITB_INDEXED: u8 = 0xe5;
pub const OP_BITB_EXTENDED: u8 = 0xf5;

pub const OP_CLRA: u8 = 0x4f;

pub const OP_CLRB: u8 = 0x5f;

pub const OP_CLR_DIRECT: u8 = 0x0f;
pub const OP_CLR_INDEXED: u8 = 0x6f;
pub const OP_CLR_EXTENDED: u8 = 0x7f;

pub const OP_CMPA_IMMEDIATE: u8 = 0x81;
pub const OP_CMPA_DIRECT: u8 = 0x91;
pub const OP_CMPA_INDEXED: u8 = 0xa1;
pub const OP_CMPA_EXTENDED: u8 = 0xb1;

pub const OP_CMPB_IMMEDIATE: u8 = 0xc1;
pub const OP_CMPB_DIRECT: u8 = 0xd1;
pub const OP_CMPB_INDEXED: u8 = 0xe1;
pub const OP_CMPB_EXTENDED: u8 = 0xf1;

pub const OP_CMPD_IMMEDIATE: u8 = 0x83;
pub const OP_CMPD_DIRECT: u8 = 0x93;
pub const OP_CMPD_INDEXED: u8 = 0xa3;
pub const OP_CMPD_EXTENDED: u8 = 0xb3;

pub const OP_CMPS_IMMEDIATE: u8 = 0x8c;
pub const OP_CMPS_DIRECT: u8 = 0x9c;
pub const OP_CMPS_INDEXED: u8 = 0xac;
pub const OP_CMPS_EXTENDED: u8 = 0xbc;

pub const OP_CMPU_IMMEDIATE: u8 = 0x83;
pub const OP_CMPU_DIRECT: u8 = 0x93;
pub const OP_CMPU_INDEXED: u8 = 0xa3;
pub const OP_CMPU_EXTENDED: u8 = 0xb3;

pub const OP_CMPX_IMMEDIATE: u8 = 0x8c;
pub const OP_CMPX_DIRECT: u8 = 0x9c;
pub const OP_CMPX_INDEXED: u8 = 0xac;
pub const OP_CMPX_EXTENDED: u8 = 0xbc;

pub const OP_CMPY_IMMEDIATE: u8 = 0x8c;
pub const OP_CMPY_DIRECT: u8 = 0x9c;
pub const OP_CMPY_INDEXED: u8 = 0xac;
pub const OP_CMPY_EXTENDED: u8 = 0xbc;

pub const OP_LSRA: u8 = 0x44;

pub const OP_LSRB: u8 = 0x54;

pub const OP_LSR_DIRECT: u8 = 0x04;
pub const OP_LSR_INDEXED: u8 = 0x64;
pub const OP_LSR_EXTENDED: u8 = 0x74;

pub const OP_MUL: u8 = 0x3d;

pub const OP_NEGA: u8 = 0x40;

pub const OP_NEGB: u8 = 0x50;

pub const OP_NEG_DIRECT: u8 = 0x00;
pub const OP_NEG_INDEXED: u8 = 0x60;
pub const OP_NEG_EXTENDED: u8 = 0x70;

pub const OP_NOP: u8 = 0x12;

pub const OP_ORA_IMMEDIATE: u8 = 0x8a;
pub const OP_ORA_DIRECT: u8 = 0x9a;
pub const OP_ORA_INDEXED: u8 = 0xaa;
pub const OP_ORA_EXTENDED: u8 = 0xba;

pub const OP_ORB_IMMEDIATE: u8 = 0xca;
pub const OP_ORB_DIRECT: u8 = 0xda;
pub const OP_ORB_INDEXED: u8 = 0xea;
pub const OP_ORB_EXTENDED: u8 = 0xfa;

pub const OP_ORCC_IMMEDIATE: u8 = 0x1a;

pub const OP_PSHS_IMMEDIATE: u8 = 0x34;

pub const OP_PSHU_IMMEDIATE: u8 = 0x36;

pub const OP_PULS_IMMEDIATE: u8 = 0x35;

pub const OP_PULU_IMMEDIATE: u8 = 0x37;

pub const OP_ROLA: u8 = 0x49;

pub const OP_ROLB: u8 = 0x59;

pub const OP_ROL_DIRECT: u8 = 0x09;
pub const OP_ROL_INDEXED: u8 = 0x69;
pub const OP_ROL_EXTENDED: u8 = 0x79;

pub const OP_RORA: u8 = 0x46;

pub const OP_RORB: u8 = 0x56;

pub const OP_ROR_DIRECT: u8 = 0x06;
pub const OP_ROR_INDEXED: u8 = 0x66;
pub const OP_ROR_EXTENDED: u8 = 0x76;

pub const OP_RTI: u8 = 0x3b;

pub const OP_RTS: u8 = 0x39;

pub const OP_SBCA_IMMEDIATE: u8 = 0x82;
pub const OP_SBCA_DIRECT: u8 = 0x92;
pub const OP_SBCA_INDEXED: u8 = 0xa2;
pub const OP_SBCA_EXTENDED: u8 = 0xb2;

pub const OP_SBCB_IMMEDIATE: u8 = 0xc2;
pub const OP_SBCB_DIRECT: u8 = 0xd2;
pub const OP_SBCB_INDEXED: u8 = 0xe2;
pub const OP_SBCB_EXTENDED: u8 = 0xf2;

pub const OP_SEX: u8 = 0x1d;

pub const OP_COMA: u8 = 0x43;

pub const OP_COMB: u8 = 0x53;

pub const OP_COM_DIRECT: u8 = 0x03;
pub const OP_COM_INDEXED: u8 = 0x63;
pub const OP_COM_EXTENDED: u8 = 0x73;

pub const OP_CWAI_IMMEDIATE: u8 = 0x3c;

pub const OP_DAA: u8 = 0x19;

pub const OP_DECA: u8 = 0x4a;

pub const OP_DECB: u8 = 0x5a;

pub const OP_DEC_DIRECT: u8 = 0x0a;
pub const OP_DEC_INDEXED: u8 = 0x6a;
pub const OP_DEC_EXTENDED: u8 = 0x7a;

pub const OP_EORA_IMMEDIATE: u8 = 0x88;
pub const OP_EORA_DIRECT: u8 = 0x98;
pub const OP_EORA_INDEXED: u8 = 0xa8;
pub const OP_EORA_EXTENDED: u8 = 0xb8;

pub const OP_EORB_IMMEDIATE: u8 = 0xc8;
pub const OP_EORB_DIRECT: u8 = 0xd8;
pub const OP_EORB_INDEXED: u8 = 0xe8;
pub const OP_EORB_EXTENDED: u8 = 0xf8;

pub const OP_EXG_IMMEDIATE: u8 = 0x1e;

pub const OP_INCA: u8 = 0x4c;

pub const OP_INCB: u8 = 0x5c;

pub const OP_INC_DIRECT: u8 = 0x0c;
pub const OP_INC_INDEXED: u8 = 0x6c;
pub const OP_INC_EXTENDED: u8 = 0x7c;

pub const OP_JMP_DIRECT: u8 = 0x0e;
pub const OP_JMP_INDEXED: u8 = 0x6e;
pub const OP_JMP_EXTENDED: u8 = 0x7e;

pub const OP_JSR_DIRECT: u8 = 0x9d;
pub const OP_JSR_INDEXED: u8 = 0xad;
pub const OP_JSR_EXTENDED: u8 = 0xbd;

pub const OP_LDA_IMMEDIATE: u8 = 0x86;
pub const OP_LDA_DIRECT: u8 = 0x96;
pub const OP_LDA_INDEXED: u8 = 0xa6;
pub const OP_LDA_EXTENDED: u8 = 0xb6;

pub const OP_LDB_IMMEDIATE: u8 = 0xc6;
pub const OP_LDB_DIRECT: u8 = 0xd6;
pub const OP_LDB_INDEXED: u8 = 0xe6;
pub const OP_LDB_EXTENDED: u8 = 0xf6;

pub const OP_LDD_IMMEDIATE: u8 = 0xcc;
pub const OP_LDD_DIRECT: u8 = 0xdc;
pub const OP_LDD_INDEXED: u8 = 0xec;
pub const OP_LDD_EXTENDED: u8 = 0xfc;

pub const OP_LDS_IMMEDIATE: u8 = 0xce;
pub const OP_LDS_DIRECT: u8 = 0xde;
pub const OP_LDS_INDEXED: u8 = 0xee;
pub const OP_LDS_EXTENDED: u8 = 0xfe;

pub const OP_LDU_IMMEDIATE: u8 = 0xce;
pub const OP_LDU_DIRECT: u8 = 0xde;
pub const OP_LDU_INDEXED: u8 = 0xee;
pub const OP_LDU_EXTENDED: u8 = 0xfe;

pub const OP_LDX_IMMEDIATE: u8 = 0x8e;
pub const OP_LDX_DIRECT: u8 = 0x9e;
pub const OP_LDX_INDEXED: u8 = 0xae;
pub const OP_LDX_EXTENDED: u8 = 0xbe;

pub const OP_LDY_IMMEDIATE: u8 = 0x8e;
pub const OP_LDY_DIRECT: u8 = 0x9e;
pub const OP_LDY_INDEXED: u8 = 0xae;
pub const OP_LDY_EXTENDED: u8 = 0xbe;

pub const OP_LEAS_INDEXED: u8 = 0x32;

pub const OP_LEAU_INDEXED: u8 = 0x33;

pub const OP_LEAX_INDEXED: u8 = 0x30;

pub const OP_LEAY_INDEXED: u8 = 0x31;

pub const OP_STA_DIRECT: u8 = 0x97;
pub const OP_STA_INDEXED: u8 = 0xa7;
pub const OP_STA_EXTENDED: u8 = 0xb7;

pub const OP_STB_DIRECT: u8 = 0xd7;
pub const OP_STB_INDEXED: u8 = 0xe7;
pub const OP_STB_EXTENDED: u8 = 0xf7;

pub const OP_STD_DIRECT: u8 = 0xdd;
pub const OP_STD_INDEXED: u8 = 0xed;
pub const OP_STD_EXTENDED: u8 = 0xfd;

pub const OP_STU_DIRECT: u8 = 0xdf;
pub const OP_STU_INDEXED: u8 = 0xef;
pub const OP_STU_EXTENDED: u8 = 0xff;

pub const OP_STX_DIRECT: u8 = 0x9f;
pub const OP_STX_INDEXED: u8 = 0xaf;
pub const OP_STX_EXTENDED: u8 = 0xbf;

pub const OP_STY_DIRECT: u8 = 0x9f;
pub const OP_STY_INDEXED: u8 = 0xaf;
pub const OP_STY_EXTENDED: u8 = 0xbf;

pub const OP_SUBA_IMMEDIATE: u8 = 0x80;
pub const OP_SUBA_DIRECT: u8 = 0x90;
pub const OP_SUBA_INDEXED: u8 = 0xa0;
pub const OP_SUBA_EXTENDED: u8 = 0xb0;

pub const OP_SUBB_IMMEDIATE: u8 = 0xc0;
pub const OP_SUBB_DIRECT: u8 = 0xd0;
pub const OP_SUBB_INDEXED: u8 = 0xe0;
pub const OP_SUBB_EXTENDED: u8 = 0xf0;

pub const OP_SUBD_IMMEDIATE: u8 = 0x83;
pub const OP_SUBD_DIRECT: u8 = 0x93;
pub const OP_SUBD_INDEXED: u8 = 0xa3;
pub const OP_SUBD_EXTENDED: u8 = 0xb3;

pub const OP_SWI: u8 = 0x3f;

pub const OP_SWI2: u8 = 0x3f;

pub const OP_SWI3: u8 = 0x3f;

pub const OP_SYNC: u8 = 0x13;

pub const OP_TFR1_IMMEDIATE: u8 = 0x1f;

pub const OP_TSTA: u8 = 0x4d;

pub const OP_TSTB: u8 = 0x5d;

pub const OP_TST_DIRECT: u8 = 0x0d;
pub const OP_TST_INDEXED: u8 = 0x6d;
pub const OP_TST_EXTENDED: u8 = 0x7d;

#[derive(Debug)]
pub enum Opcode {
	Abx,
	Adca,
	Adcb,
	Adda,
	Addb,
	Addd,
	Anda,
	Andb,
	Andcc,
	Asla,
	Aslb,
	Asl,
	Asra,
	Asrb,
	Asr,
	Bita,
	Bitb,
	Clra,
	Clrb,
	Clr,
	Cmpa,
	Cmpb,
	Cmpd,
	Cmps,
	Cmpu,
	Cmpx,
	Cmpy,
	Lsra,
	Lsrb,
	Lsr,
	Mul,
	Nega,
	Negb,
	Neg,
	Nop,
	Ora,
	Orb,
	Orcc,
	Pshs,
	Pshu,
	Puls,
	Pulu,
	Rola,
	Rolb,
	Rol,
	Rora,
	Rorb,
	Ror,
	Rti,
	Rts,
	Sbca,
	Sbcb,
	Sex,
	Coma,
	Comb,
	Com,
	Cwai,
	Daa,
	Deca,
	Decb,
	Dec,
	Eora,
	Eorb,
	Exg,
	Inca,
	Incb,
	Inc,
	Jmp,
	Jsr,
	Lda,
	Ldb,
	Ldd,
	Lds,
	Ldu,
	Ldx,
	Ldy,
	Leas,
	Leau,
	Leax,
	Leay,
	Sta,
	Stb,
	Std,
	Stu,
	Stx,
	Sty,
	Suba,
	Subb,
	Subd,
	Swi,
	Swi2,
	Swi3,
	Sync,
	Tfr1,
	Tsta,
	Tstb,
	Tst,
}

impl Opcode {
	pub fn from_u16(op_16: u16) -> Opcode {
		let hi_op = (op_16 >> 8) as u8;
		match hi_op {
			OP_PAGE_2 => {
				let op = op_16 as u8;
				match op {
					  OP_CMPD_IMMEDIATE
					| OP_CMPD_DIRECT
					| OP_CMPD_INDEXED
					| OP_CMPD_EXTENDED
					=> Opcode::Cmpd,

					  OP_CMPY_IMMEDIATE
					| OP_CMPY_DIRECT
					| OP_CMPY_INDEXED
					| OP_CMPY_EXTENDED
					=> Opcode::Cmpy,

					  OP_LDS_IMMEDIATE
					| OP_LDS_DIRECT
					| OP_LDS_INDEXED
					| OP_LDS_EXTENDED
					=> Opcode::Lds,

					  OP_LDY_IMMEDIATE
					| OP_LDY_DIRECT
					| OP_LDY_INDEXED
					| OP_LDY_EXTENDED
					=> Opcode::Ldy,

					  OP_STY_DIRECT
					| OP_STY_INDEXED
					| OP_STY_EXTENDED
					=> Opcode::Sty,

					OP_SWI2 => Opcode::Swi2,

					
					_ => panic!("Unknown opcode 0x{:02x}", op)
				}
			},
			OP_PAGE_3 => {
				let op = op_16 as u8;

				match op {
					  OP_CMPS_IMMEDIATE
					| OP_CMPS_DIRECT
					| OP_CMPS_INDEXED
					| OP_CMPS_EXTENDED
					=> Opcode::Cmps,

					  OP_CMPU_IMMEDIATE
					| OP_CMPU_DIRECT
					| OP_CMPU_INDEXED
					| OP_CMPU_EXTENDED
					=> Opcode::Cmpu,

					OP_SWI3 => Opcode::Swi3,

					_ => panic!("Unknown opcode 0x{:02x}", op)
				}
			}
			_ => Self::from_u8(hi_op)
		}
	}

	pub fn from_u8(op: u8) -> Opcode {
		match op {
			OP_ABX => Opcode::Abx,

			  OP_ADCA_IMMEDIATE
			| OP_ADCA_DIRECT
			| OP_ADCA_INDEXED
			| OP_ADCA_EXTENDED
			=> Opcode::Adca,

			  OP_ADCB_IMMEDIATE
			| OP_ADCB_DIRECT
			| OP_ADCB_INDEXED
			| OP_ADCB_EXTENDED
			=> Opcode::Adcb,

			  OP_ADDA_IMMEDIATE
			| OP_ADDA_DIRECT
			| OP_ADDA_INDEXED
			| OP_ADDA_EXTENDED
			=> Opcode::Adda,

			  OP_ADDB_IMMEDIATE
			| OP_ADDB_DIRECT
			| OP_ADDB_INDEXED
			| OP_ADDB_EXTENDED
			=> Opcode::Addb,

			  OP_ADDD_IMMEDIATE
			| OP_ADDD_DIRECT
			| OP_ADDD_INDEXED
			| OP_ADDD_EXTENDED
			=> Opcode::Addd,

			  OP_ANDA_IMMEDIATE
			| OP_ANDA_DIRECT
			| OP_ANDA_INDEXED
			| OP_ANDA_EXTENDED
			=> Opcode::Anda,

			  OP_ANDB_IMMEDIATE
			| OP_ANDB_DIRECT
			| OP_ANDB_INDEXED
			| OP_ANDB_EXTENDED
			=> Opcode::Andb,

			OP_ANDCC_IMMEDIATE => Opcode::Andcc,
			OP_ASLA => Opcode::Asla,
			OP_ASLB => Opcode::Aslb,

			  OP_ASL_DIRECT
			| OP_ASL_INDEXED
			| OP_ASL_EXTENDED
			=> Opcode::Asl,

			OP_ASRA => Opcode::Asra,
			OP_ASRB => Opcode::Asrb,

			  OP_ASR_DIRECT
			| OP_ASR_INDEXED
			| OP_ASR_EXTENDED
			=> Opcode::Asr,

			  OP_BITA_IMMEDIATE
			| OP_BITA_DIRECT
			| OP_BITA_INDEXED
			| OP_BITA_EXTENDED
			=> Opcode::Bita,

			  OP_BITB_IMMEDIATE
			| OP_BITB_DIRECT
			| OP_BITB_INDEXED
			| OP_BITB_EXTENDED
			=> Opcode::Bitb,

			OP_CLRA => Opcode::Clra,
			OP_CLRB => Opcode::Clrb,

			  OP_CLR_DIRECT
			| OP_CLR_INDEXED
			| OP_CLR_EXTENDED
			=> Opcode::Clr,

			  OP_CMPA_IMMEDIATE
			| OP_CMPA_DIRECT
			| OP_CMPA_INDEXED
			| OP_CMPA_EXTENDED
			=> Opcode::Cmpa,

			  OP_CMPB_IMMEDIATE
			| OP_CMPB_DIRECT
			| OP_CMPB_INDEXED
			| OP_CMPB_EXTENDED
			=> Opcode::Cmpb,

			  OP_CMPX_IMMEDIATE
			| OP_CMPX_DIRECT
			| OP_CMPX_INDEXED
			| OP_CMPX_EXTENDED
			=> Opcode::Cmpx,

			OP_LSRA => Opcode::Lsra,
			OP_LSRB => Opcode::Lsrb,

			  OP_LSR_DIRECT
			| OP_LSR_INDEXED
			| OP_LSR_EXTENDED
			=> Opcode::Lsr,

			OP_MUL => Opcode::Mul,
			OP_NEGA => Opcode::Nega,
			OP_NEGB => Opcode::Negb,

			  OP_NEG_DIRECT
			| OP_NEG_INDEXED
			| OP_NEG_EXTENDED
			=> Opcode::Neg,

			OP_NOP => Opcode::Nop,

			  OP_ORA_IMMEDIATE
			| OP_ORA_DIRECT
			| OP_ORA_INDEXED
			| OP_ORA_EXTENDED
			=> Opcode::Ora,

			  OP_ORB_IMMEDIATE
			| OP_ORB_DIRECT
			| OP_ORB_INDEXED
			| OP_ORB_EXTENDED
			=> Opcode::Orb,

			OP_ORCC_IMMEDIATE => Opcode::Orcc,
			OP_PSHS_IMMEDIATE => Opcode::Pshs,
			OP_PSHU_IMMEDIATE => Opcode::Pshu,
			OP_PULS_IMMEDIATE => Opcode::Puls,
			OP_PULU_IMMEDIATE => Opcode::Pulu,
			OP_ROLA => Opcode::Rola,
			OP_ROLB => Opcode::Rolb,

			  OP_ROL_DIRECT
			| OP_ROL_INDEXED
			| OP_ROL_EXTENDED
			=> Opcode::Rol,

			OP_RORA => Opcode::Rora,
			OP_RORB => Opcode::Rorb,

			  OP_ROR_DIRECT
			| OP_ROR_INDEXED
			| OP_ROR_EXTENDED
			=> Opcode::Ror,

			OP_RTI => Opcode::Rti,
			OP_RTS => Opcode::Rts,

			  OP_SBCA_IMMEDIATE
			| OP_SBCA_DIRECT
			| OP_SBCA_INDEXED
			| OP_SBCA_EXTENDED
			=> Opcode::Sbca,

			  OP_SBCB_IMMEDIATE
			| OP_SBCB_DIRECT
			| OP_SBCB_INDEXED
			| OP_SBCB_EXTENDED
			=> Opcode::Sbcb,

			OP_SEX => Opcode::Sex,
			OP_COMA => Opcode::Coma,
			OP_COMB => Opcode::Comb,

			  OP_COM_DIRECT
			| OP_COM_INDEXED
			| OP_COM_EXTENDED
			=> Opcode::Com,

			OP_CWAI_IMMEDIATE => Opcode::Cwai,
			OP_DAA => Opcode::Daa,
			OP_DECA => Opcode::Deca,
			OP_DECB => Opcode::Decb,

			  OP_DEC_DIRECT
			| OP_DEC_INDEXED
			| OP_DEC_EXTENDED
			=> Opcode::Dec,

			  OP_EORA_IMMEDIATE
			| OP_EORA_DIRECT
			| OP_EORA_INDEXED
			| OP_EORA_EXTENDED
			=> Opcode::Eora,

			  OP_EORB_IMMEDIATE
			| OP_EORB_DIRECT
			| OP_EORB_INDEXED
			| OP_EORB_EXTENDED
			=> Opcode::Eorb,

			OP_EXG_IMMEDIATE => Opcode::Exg,
			OP_INCA => Opcode::Inca,
			OP_INCB => Opcode::Incb,

			  OP_INC_DIRECT
			| OP_INC_INDEXED
			| OP_INC_EXTENDED
			=> Opcode::Inc,

			  OP_JMP_DIRECT
			| OP_JMP_INDEXED
			| OP_JMP_EXTENDED
			=> Opcode::Jmp,

			  OP_JSR_DIRECT
			| OP_JSR_INDEXED
			| OP_JSR_EXTENDED
			=> Opcode::Jsr,

			  OP_LDA_IMMEDIATE
			| OP_LDA_DIRECT
			| OP_LDA_INDEXED
			| OP_LDA_EXTENDED
			=> Opcode::Lda,

			  OP_LDB_IMMEDIATE
			| OP_LDB_DIRECT
			| OP_LDB_INDEXED
			| OP_LDB_EXTENDED
			=> Opcode::Ldb,

			  OP_LDD_IMMEDIATE
			| OP_LDD_DIRECT
			| OP_LDD_INDEXED
			| OP_LDD_EXTENDED
			=> Opcode::Ldd,

			  OP_LDU_IMMEDIATE
			| OP_LDU_DIRECT
			| OP_LDU_INDEXED
			| OP_LDU_EXTENDED
			=> Opcode::Ldu,

			  OP_LDX_IMMEDIATE
			| OP_LDX_DIRECT
			| OP_LDX_INDEXED
			| OP_LDX_EXTENDED
			=> Opcode::Ldx,

			OP_LEAS_INDEXED => Opcode::Leas,
			OP_LEAU_INDEXED => Opcode::Leau,
			OP_LEAX_INDEXED => Opcode::Leax,
			OP_LEAY_INDEXED => Opcode::Leay,

			  OP_STA_DIRECT
			| OP_STA_INDEXED
			| OP_STA_EXTENDED
			=> Opcode::Sta,

			  OP_STB_DIRECT
			| OP_STB_INDEXED
			| OP_STB_EXTENDED
			=> Opcode::Stb,

			  OP_STD_DIRECT
			| OP_STD_INDEXED
			| OP_STD_EXTENDED
			=> Opcode::Std,

			  OP_STU_DIRECT
			| OP_STU_INDEXED
			| OP_STU_EXTENDED
			=> Opcode::Stu,

			  OP_STX_DIRECT
			| OP_STX_INDEXED
			| OP_STX_EXTENDED
			=> Opcode::Stx,

			  OP_SUBA_IMMEDIATE
			| OP_SUBA_DIRECT
			| OP_SUBA_INDEXED
			| OP_SUBA_EXTENDED
			=> Opcode::Suba,

			  OP_SUBB_IMMEDIATE
			| OP_SUBB_DIRECT
			| OP_SUBB_INDEXED
			| OP_SUBB_EXTENDED
			=> Opcode::Subb,

			  OP_SUBD_IMMEDIATE
			| OP_SUBD_DIRECT
			| OP_SUBD_INDEXED
			| OP_SUBD_EXTENDED
			=> Opcode::Subd,

			OP_SWI => Opcode::Swi,
			OP_SYNC => Opcode::Sync,
			OP_TFR1_IMMEDIATE => Opcode::Tfr1,
			OP_TSTA => Opcode::Tsta,
			OP_TSTB => Opcode::Tstb,

			  OP_TST_DIRECT
			| OP_TST_INDEXED
			| OP_TST_EXTENDED
			=> Opcode::Tst,

			_ => panic!("Unknown opcode 0x{:02x}", op)
		}
	}
}
