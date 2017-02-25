use memory::Memory;
use super::instruction::{Instruction, Mnemonic, Addressing, IndexMode, IndexRegister};

fn instr(mnemonic: Mnemonic, addressing: Addressing) -> Option<Instruction> {
	Some(Instruction(mnemonic, addressing))
}

fn inherent() -> Addressing {
	Addressing::Inherent
}

fn immediate8(mem: &Memory, addr: u16) -> Addressing {
	Addressing::Immediate8(mem.read_u8(addr))
}

fn immediate16(mem: &Memory, addr: u16) -> Addressing {
	Addressing::Immediate16(mem.read_u16(addr))
}

fn relative8(mem: &Memory, addr: u16) -> Addressing {
	Addressing::Relative8(mem.read_u8(addr) as i8)
}

fn relative16(mem: &Memory, addr: u16) -> Addressing {
	Addressing::Relative16(mem.read_u16(addr) as i16)
}

fn direct(mem: &Memory, addr: u16) -> Addressing {
	Addressing::Direct(mem.read_u8(addr))
}

fn extended(mem: &Memory, addr: u16) -> Addressing {
	Addressing::Extended(mem.read_u16(addr))
}

fn parse_indexed(mem: &Memory, addr: u16) -> IndexMode {
	let postbyte = mem.read_u8(addr);
	let addr = addr.wrapping_add(1);

	let reg_nibble = (postbyte & 0b0110_0000) >> 5;
	let reg = IndexRegister::from(reg_nibble);

	if postbyte >> 7 == 0 {
		// The postbyte itself contains a 5-bit offset
		let mut offset = postbyte & 0b0001_1111;

		// Sign-extend 5-bit offset, if i've understood sign extension correctly
		if offset >> 4 == 1 {
			offset |= 0b1110_0000;
		}
		let offset = offset as i8;

		return IndexMode::Offset5 { reg: reg, offset: offset }
	}

	match postbyte & 0b0001_1111 {
		0 => return IndexMode::Increment1 { reg: reg },
		1 => return IndexMode::Decrement1 { reg: reg },
		_ => ()
	}

	let indirect = 1 == ((postbyte & 0b0001_0000) >> 4);
	let index_op = postbyte & 0b1111;

	match index_op {
		0b0001 => IndexMode::Increment2 { reg: reg, indirect: indirect },
		0b0011 => IndexMode::Decrement2 { reg: reg, indirect: indirect },

		0b0100 => IndexMode::Offset0 { reg: reg, indirect: indirect },

		0b0110 => IndexMode::OffsetA { reg: reg, indirect: indirect },
		0b0101 => IndexMode::OffsetB { reg: reg, indirect: indirect },
		0b1011 => IndexMode::OffsetD { reg: reg, indirect: indirect },

		0b1000 => IndexMode::Offset8 {
			reg: reg,
			offset: mem.read_u8(addr) as i8,
			indirect: indirect
		},
		0b1001 => IndexMode::Offset16 {
			reg: reg,
			offset: mem.read_u16(addr) as i16,
			indirect: indirect
		},

		0b1100 => IndexMode::PcOffset8 {
			reg: reg,
			offset: mem.read_u8(addr) as i8,
			indirect: indirect
		},
		0b1101 => IndexMode::PcOffset16 {
			reg: reg,
			offset: mem.read_u16(addr) as i16,
			indirect: indirect
		},

		0b1111 => IndexMode::ExtendedIndirect {
			address: mem.read_u16(addr)
		},

		_ => unreachable!()
	}
}

fn indexed(mem: &Memory, addr: u16) -> Addressing {
	Addressing::Indexed(parse_indexed(mem, addr))
}

pub fn parse_instruction(mem: &Memory, addr: u16) -> Option<Instruction> {
	const PAGE_2: u8 = 0x10;
	const PAGE_3: u8 = 0x11;

	let mut pc = addr;
	let op = mem.read_u8(addr);
	
	match op {
		PAGE_2 => {
			pc += 1;
			let op = mem.read_u8(pc);
			match op {
				0x83 => instr(Mnemonic::Cmpd, immediate16(mem, pc)),
				0x93 => instr(Mnemonic::Cmpd, direct(mem, pc)),
				0xa3 => instr(Mnemonic::Cmpd, indexed(mem, pc)),
				0xb3 => instr(Mnemonic::Cmpd, extended(mem, pc)),
				0x8c => instr(Mnemonic::Cmpy, immediate16(mem, pc)),
				0x9c => instr(Mnemonic::Cmpy, direct(mem, pc)),
				0xac => instr(Mnemonic::Cmpy, indexed(mem, pc)),
				0xbc => instr(Mnemonic::Cmpy, extended(mem, pc)),
				0xce => instr(Mnemonic::Lds, immediate16(mem, pc)),
				0xde => instr(Mnemonic::Lds, direct(mem, pc)),
				0xee => instr(Mnemonic::Lds, indexed(mem, pc)),
				0xfe => instr(Mnemonic::Lds, extended(mem, pc)),
				0x8e => instr(Mnemonic::Ldy, immediate16(mem, pc)),
				0x9e => instr(Mnemonic::Ldy, direct(mem, pc)),
				0xae => instr(Mnemonic::Ldy, indexed(mem, pc)),
				0xbe => instr(Mnemonic::Ldy, extended(mem, pc)),
				0x9f => instr(Mnemonic::Sty, direct(mem, pc)),
				0xaf => instr(Mnemonic::Sty, indexed(mem, pc)),
				0xbf => instr(Mnemonic::Sty, extended(mem, pc)),
				0x3f => instr(Mnemonic::Swi2, inherent()),
				0x27 => instr(Mnemonic::Beq, relative16(mem, pc)),
				0x2c => instr(Mnemonic::Bge, relative16(mem, pc)),
				0x2e => instr(Mnemonic::Bgt, relative16(mem, pc)),
				0x22 => instr(Mnemonic::Bhi, relative16(mem, pc)),
				0x24 => instr(Mnemonic::Bhs, relative16(mem, pc)),
				0x2f => instr(Mnemonic::Ble, relative16(mem, pc)),
				0x25 => instr(Mnemonic::Blo, relative16(mem, pc)),
				0x23 => instr(Mnemonic::Bls, relative16(mem, pc)),
				0x2d => instr(Mnemonic::Blt, relative16(mem, pc)),
				0x2b => instr(Mnemonic::Bmi, relative16(mem, pc)),
				0x26 => instr(Mnemonic::Bne, relative16(mem, pc)),
				0x2a => instr(Mnemonic::Bpl, relative16(mem, pc)),
				0x21 => instr(Mnemonic::Brn, relative16(mem, pc)),
				0x28 => instr(Mnemonic::Bvc, relative16(mem, pc)),
				0x29 => instr(Mnemonic::Bvs, relative16(mem, pc)),

				_ => None
			}
		},
		PAGE_3 => {
			pc += 1;
			let op = mem.read_u8(pc);
			match op {
				0x8c => instr(Mnemonic::Cmps, immediate16(mem, pc)),
				0x9c => instr(Mnemonic::Cmps, direct(mem, pc)),
				0xac => instr(Mnemonic::Cmps, indexed(mem, pc)),
				0xbc => instr(Mnemonic::Cmps, extended(mem, pc)),
				0x83 => instr(Mnemonic::Cmpu, immediate16(mem, pc)),
				0x93 => instr(Mnemonic::Cmpu, direct(mem, pc)),
				0xa3 => instr(Mnemonic::Cmpu, indexed(mem, pc)),
				0xb3 => instr(Mnemonic::Cmpu, extended(mem, pc)),
				0x3f => instr(Mnemonic::Swi3, inherent()),

				_ => None
			}
		},
		0x3a => instr(Mnemonic::Abx, inherent()),
		0x89 => instr(Mnemonic::Adca, immediate8(mem, pc)),
		0x99 => instr(Mnemonic::Adca, direct(mem, pc)),
		0xa9 => instr(Mnemonic::Adca, indexed(mem, pc)),
		0xb9 => instr(Mnemonic::Adca, extended(mem, pc)),
		0xc9 => instr(Mnemonic::Adcb, immediate8(mem, pc)),
		0xd9 => instr(Mnemonic::Adcb, direct(mem, pc)),
		0xe9 => instr(Mnemonic::Adcb, indexed(mem, pc)),
		0xf9 => instr(Mnemonic::Adcb, extended(mem, pc)),
		0x8b => instr(Mnemonic::Adda, immediate8(mem, pc)),
		0x9b => instr(Mnemonic::Adda, direct(mem, pc)),
		0xab => instr(Mnemonic::Adda, indexed(mem, pc)),
		0xbb => instr(Mnemonic::Adda, extended(mem, pc)),
		0xcb => instr(Mnemonic::Addb, immediate8(mem, pc)),
		0xdb => instr(Mnemonic::Addb, direct(mem, pc)),
		0xeb => instr(Mnemonic::Addb, indexed(mem, pc)),
		0xfb => instr(Mnemonic::Addb, extended(mem, pc)),
		0xc3 => instr(Mnemonic::Addd, immediate16(mem, pc)),
		0xd3 => instr(Mnemonic::Addd, direct(mem, pc)),
		0xe3 => instr(Mnemonic::Addd, indexed(mem, pc)),
		0xf3 => instr(Mnemonic::Addd, extended(mem, pc)),
		0x84 => instr(Mnemonic::Anda, immediate8(mem, pc)),
		0x94 => instr(Mnemonic::Anda, direct(mem, pc)),
		0xa4 => instr(Mnemonic::Anda, indexed(mem, pc)),
		0xb4 => instr(Mnemonic::Anda, extended(mem, pc)),
		0xc4 => instr(Mnemonic::Andb, immediate8(mem, pc)),
		0xd4 => instr(Mnemonic::Andb, direct(mem, pc)),
		0xe4 => instr(Mnemonic::Andb, indexed(mem, pc)),
		0xf4 => instr(Mnemonic::Andb, extended(mem, pc)),
		0x1c => instr(Mnemonic::Andcc, immediate8(mem, pc)),
		0x48 => instr(Mnemonic::Asla, inherent()),
		0x58 => instr(Mnemonic::Aslb, inherent()),
		0x08 => instr(Mnemonic::Asl, direct(mem, pc)),
		0x68 => instr(Mnemonic::Asl, indexed(mem, pc)),
		0x78 => instr(Mnemonic::Asl, extended(mem, pc)),
		0x47 => instr(Mnemonic::Asra, inherent()),
		0x57 => instr(Mnemonic::Asrb, inherent()),
		0x07 => instr(Mnemonic::Asr, direct(mem, pc)),
		0x67 => instr(Mnemonic::Asr, indexed(mem, pc)),
		0x77 => instr(Mnemonic::Asr, extended(mem, pc)),
		0x85 => instr(Mnemonic::Bita, immediate8(mem, pc)),
		0x95 => instr(Mnemonic::Bita, direct(mem, pc)),
		0xa5 => instr(Mnemonic::Bita, indexed(mem, pc)),
		0xb5 => instr(Mnemonic::Bita, extended(mem, pc)),
		0xc5 => instr(Mnemonic::Bitb, immediate8(mem, pc)),
		0xd5 => instr(Mnemonic::Bitb, direct(mem, pc)),
		0xe5 => instr(Mnemonic::Bitb, indexed(mem, pc)),
		0xf5 => instr(Mnemonic::Bitb, extended(mem, pc)),
		0x4f => instr(Mnemonic::Clra, inherent()),
		0x5f => instr(Mnemonic::Clrb, inherent()),
		0x0f => instr(Mnemonic::Clr, direct(mem, pc)),
		0x6f => instr(Mnemonic::Clr, indexed(mem, pc)),
		0x7f => instr(Mnemonic::Clr, extended(mem, pc)),
		0x81 => instr(Mnemonic::Cmpa, immediate8(mem, pc)),
		0x91 => instr(Mnemonic::Cmpa, direct(mem, pc)),
		0xa1 => instr(Mnemonic::Cmpa, indexed(mem, pc)),
		0xb1 => instr(Mnemonic::Cmpa, extended(mem, pc)),
		0xc1 => instr(Mnemonic::Cmpb, immediate8(mem, pc)),
		0xd1 => instr(Mnemonic::Cmpb, direct(mem, pc)),
		0xe1 => instr(Mnemonic::Cmpb, indexed(mem, pc)),
		0xf1 => instr(Mnemonic::Cmpb, extended(mem, pc)),
		0x8c => instr(Mnemonic::Cmpx, immediate16(mem, pc)),
		0x9c => instr(Mnemonic::Cmpx, direct(mem, pc)),
		0xac => instr(Mnemonic::Cmpx, indexed(mem, pc)),
		0xbc => instr(Mnemonic::Cmpx, extended(mem, pc)),
		0x44 => instr(Mnemonic::Lsra, inherent()),
		0x54 => instr(Mnemonic::Lsrb, inherent()),
		0x04 => instr(Mnemonic::Lsr, direct(mem, pc)),
		0x64 => instr(Mnemonic::Lsr, indexed(mem, pc)),
		0x74 => instr(Mnemonic::Lsr, extended(mem, pc)),
		0x3d => instr(Mnemonic::Mul, inherent()),
		0x40 => instr(Mnemonic::Nega, inherent()),
		0x50 => instr(Mnemonic::Negb, inherent()),
		0x00 => instr(Mnemonic::Neg, direct(mem, pc)),
		0x60 => instr(Mnemonic::Neg, indexed(mem, pc)),
		0x70 => instr(Mnemonic::Neg, extended(mem, pc)),
		0x12 => instr(Mnemonic::Nop, inherent()),
		0x8a => instr(Mnemonic::Ora, immediate8(mem, pc)),
		0x9a => instr(Mnemonic::Ora, direct(mem, pc)),
		0xaa => instr(Mnemonic::Ora, indexed(mem, pc)),
		0xba => instr(Mnemonic::Ora, extended(mem, pc)),
		0xca => instr(Mnemonic::Orb, immediate8(mem, pc)),
		0xda => instr(Mnemonic::Orb, direct(mem, pc)),
		0xea => instr(Mnemonic::Orb, indexed(mem, pc)),
		0xfa => instr(Mnemonic::Orb, extended(mem, pc)),
		0x1a => instr(Mnemonic::Orcc, immediate8(mem, pc)),
		0x34 => instr(Mnemonic::Pshs, immediate8(mem, pc)),
		0x36 => instr(Mnemonic::Pshu, immediate8(mem, pc)),
		0x35 => instr(Mnemonic::Puls, immediate8(mem, pc)),
		0x37 => instr(Mnemonic::Pulu, immediate8(mem, pc)),
		0x49 => instr(Mnemonic::Rola, inherent()),
		0x59 => instr(Mnemonic::Rolb, inherent()),
		0x09 => instr(Mnemonic::Rol, direct(mem, pc)),
		0x69 => instr(Mnemonic::Rol, indexed(mem, pc)),
		0x79 => instr(Mnemonic::Rol, extended(mem, pc)),
		0x46 => instr(Mnemonic::Rora, inherent()),
		0x56 => instr(Mnemonic::Rorb, inherent()),
		0x06 => instr(Mnemonic::Ror, direct(mem, pc)),
		0x66 => instr(Mnemonic::Ror, indexed(mem, pc)),
		0x76 => instr(Mnemonic::Ror, extended(mem, pc)),
		0x3b => instr(Mnemonic::Rti, inherent()),
		0x39 => instr(Mnemonic::Rts, inherent()),
		0x82 => instr(Mnemonic::Sbca, immediate8(mem, pc)),
		0x92 => instr(Mnemonic::Sbca, direct(mem, pc)),
		0xa2 => instr(Mnemonic::Sbca, indexed(mem, pc)),
		0xb2 => instr(Mnemonic::Sbca, extended(mem, pc)),
		0xc2 => instr(Mnemonic::Sbcb, immediate8(mem, pc)),
		0xd2 => instr(Mnemonic::Sbcb, direct(mem, pc)),
		0xe2 => instr(Mnemonic::Sbcb, indexed(mem, pc)),
		0xf2 => instr(Mnemonic::Sbcb, extended(mem, pc)),
		0x1d => instr(Mnemonic::Sex, inherent()),
		0x43 => instr(Mnemonic::Coma, inherent()),
		0x53 => instr(Mnemonic::Comb, inherent()),
		0x03 => instr(Mnemonic::Com, direct(mem, pc)),
		0x63 => instr(Mnemonic::Com, indexed(mem, pc)),
		0x73 => instr(Mnemonic::Com, extended(mem, pc)),
		0x3c => instr(Mnemonic::Cwai, immediate8(mem, pc)),
		0x19 => instr(Mnemonic::Daa, inherent()),
		0x4a => instr(Mnemonic::Deca, inherent()),
		0x5a => instr(Mnemonic::Decb, inherent()),
		0x0a => instr(Mnemonic::Dec, direct(mem, pc)),
		0x6a => instr(Mnemonic::Dec, indexed(mem, pc)),
		0x7a => instr(Mnemonic::Dec, extended(mem, pc)),
		0x88 => instr(Mnemonic::Eora, immediate8(mem, pc)),
		0x98 => instr(Mnemonic::Eora, direct(mem, pc)),
		0xa8 => instr(Mnemonic::Eora, indexed(mem, pc)),
		0xb8 => instr(Mnemonic::Eora, extended(mem, pc)),
		0xc8 => instr(Mnemonic::Eorb, immediate8(mem, pc)),
		0xd8 => instr(Mnemonic::Eorb, direct(mem, pc)),
		0xe8 => instr(Mnemonic::Eorb, indexed(mem, pc)),
		0xf8 => instr(Mnemonic::Eorb, extended(mem, pc)),
		0x1e => instr(Mnemonic::Exg, immediate8(mem, pc)),
		0x4c => instr(Mnemonic::Inca, inherent()),
		0x5c => instr(Mnemonic::Incb, inherent()),
		0x0c => instr(Mnemonic::Inc, direct(mem, pc)),
		0x6c => instr(Mnemonic::Inc, indexed(mem, pc)),
		0x7c => instr(Mnemonic::Inc, extended(mem, pc)),
		0x0e => instr(Mnemonic::Jmp, direct(mem, pc)),
		0x6e => instr(Mnemonic::Jmp, indexed(mem, pc)),
		0x7e => instr(Mnemonic::Jmp, extended(mem, pc)),
		0x9d => instr(Mnemonic::Jsr, direct(mem, pc)),
		0xad => instr(Mnemonic::Jsr, indexed(mem, pc)),
		0xbd => instr(Mnemonic::Jsr, extended(mem, pc)),
		0x86 => instr(Mnemonic::Lda, immediate8(mem, pc)),
		0x96 => instr(Mnemonic::Lda, direct(mem, pc)),
		0xa6 => instr(Mnemonic::Lda, indexed(mem, pc)),
		0xb6 => instr(Mnemonic::Lda, extended(mem, pc)),
		0xc6 => instr(Mnemonic::Ldb, immediate8(mem, pc)),
		0xd6 => instr(Mnemonic::Ldb, direct(mem, pc)),
		0xe6 => instr(Mnemonic::Ldb, indexed(mem, pc)),
		0xf6 => instr(Mnemonic::Ldb, extended(mem, pc)),
		0xcc => instr(Mnemonic::Ldd, immediate16(mem, pc)),
		0xdc => instr(Mnemonic::Ldd, direct(mem, pc)),
		0xec => instr(Mnemonic::Ldd, indexed(mem, pc)),
		0xfc => instr(Mnemonic::Ldd, extended(mem, pc)),
		0xce => instr(Mnemonic::Ldu, immediate16(mem, pc)),
		0xde => instr(Mnemonic::Ldu, direct(mem, pc)),
		0xee => instr(Mnemonic::Ldu, indexed(mem, pc)),
		0xfe => instr(Mnemonic::Ldu, extended(mem, pc)),
		0x8e => instr(Mnemonic::Ldx, immediate16(mem, pc)),
		0x9e => instr(Mnemonic::Ldx, direct(mem, pc)),
		0xae => instr(Mnemonic::Ldx, indexed(mem, pc)),
		0xbe => instr(Mnemonic::Ldx, extended(mem, pc)),
		0x32 => instr(Mnemonic::Leas, indexed(mem, pc)),
		0x33 => instr(Mnemonic::Leau, indexed(mem, pc)),
		0x30 => instr(Mnemonic::Leax, indexed(mem, pc)),
		0x31 => instr(Mnemonic::Leay, indexed(mem, pc)),
		0x97 => instr(Mnemonic::Sta, direct(mem, pc)),
		0xa7 => instr(Mnemonic::Sta, indexed(mem, pc)),
		0xb7 => instr(Mnemonic::Sta, extended(mem, pc)),
		0xd7 => instr(Mnemonic::Stb, direct(mem, pc)),
		0xe7 => instr(Mnemonic::Stb, indexed(mem, pc)),
		0xf7 => instr(Mnemonic::Stb, extended(mem, pc)),
		0xdd => instr(Mnemonic::Std, direct(mem, pc)),
		0xed => instr(Mnemonic::Std, indexed(mem, pc)),
		0xfd => instr(Mnemonic::Std, extended(mem, pc)),
		0xdf => instr(Mnemonic::Stu, direct(mem, pc)),
		0xef => instr(Mnemonic::Stu, indexed(mem, pc)),
		0xff => instr(Mnemonic::Stu, extended(mem, pc)),
		0x9f => instr(Mnemonic::Stx, direct(mem, pc)),
		0xaf => instr(Mnemonic::Stx, indexed(mem, pc)),
		0xbf => instr(Mnemonic::Stx, extended(mem, pc)),
		0x80 => instr(Mnemonic::Suba, immediate8(mem, pc)),
		0x90 => instr(Mnemonic::Suba, direct(mem, pc)),
		0xa0 => instr(Mnemonic::Suba, indexed(mem, pc)),
		0xb0 => instr(Mnemonic::Suba, extended(mem, pc)),
		0xc0 => instr(Mnemonic::Subb, immediate8(mem, pc)),
		0xd0 => instr(Mnemonic::Subb, direct(mem, pc)),
		0xe0 => instr(Mnemonic::Subb, indexed(mem, pc)),
		0xf0 => instr(Mnemonic::Subb, extended(mem, pc)),
		0x83 => instr(Mnemonic::Subd, immediate16(mem, pc)),
		0x93 => instr(Mnemonic::Subd, direct(mem, pc)),
		0xa3 => instr(Mnemonic::Subd, indexed(mem, pc)),
		0xb3 => instr(Mnemonic::Subd, extended(mem, pc)),
		0x3f => instr(Mnemonic::Swi, inherent()),
		0x13 => instr(Mnemonic::Sync, inherent()),
		0x1f => instr(Mnemonic::Tfr, immediate8(mem, pc)),
		0x4d => instr(Mnemonic::Tsta, inherent()),
		0x5d => instr(Mnemonic::Tstb, inherent()),
		0x0d => instr(Mnemonic::Tst, direct(mem, pc)),
		0x6d => instr(Mnemonic::Tst, indexed(mem, pc)),
		0x7d => instr(Mnemonic::Tst, extended(mem, pc)),
		0x27 => instr(Mnemonic::Beq, relative8(mem, pc)),
		0x2c => instr(Mnemonic::Bge, relative8(mem, pc)),
		0x2e => instr(Mnemonic::Bgt, relative8(mem, pc)),
		0x22 => instr(Mnemonic::Bhi, relative8(mem, pc)),
		0x24 => instr(Mnemonic::Bhs, relative8(mem, pc)),
		0x2f => instr(Mnemonic::Ble, relative8(mem, pc)),
		0x25 => instr(Mnemonic::Blo, relative8(mem, pc)),
		0x23 => instr(Mnemonic::Bls, relative8(mem, pc)),
		0x2d => instr(Mnemonic::Blt, relative8(mem, pc)),
		0x2b => instr(Mnemonic::Bmi, relative8(mem, pc)),
		0x26 => instr(Mnemonic::Bne, relative8(mem, pc)),
		0x2a => instr(Mnemonic::Bpl, relative8(mem, pc)),
		0x20 => instr(Mnemonic::Bra, relative8(mem, pc)),
		0x16 => instr(Mnemonic::Bra, relative16(mem, pc)),
		0x21 => instr(Mnemonic::Brn, relative8(mem, pc)),
		0x8d => instr(Mnemonic::Bsr, relative8(mem, pc)),
		0x17 => instr(Mnemonic::Bsr, relative16(mem, pc)),
		0x28 => instr(Mnemonic::Bvc, relative8(mem, pc)),
		0x29 => instr(Mnemonic::Bvs, relative8(mem, pc)),

		_ => None
	}
}