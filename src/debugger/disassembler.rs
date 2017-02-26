use std::fmt::{self, Display, Formatter};
use pack::unpack_u16;
use memory::Memory;
use super::instruction::{Instruction, Mnemonic, Addressing, IndexMode, IndexRegister};

pub struct DisassembledInstruction {
	pub instruction: Option<Instruction>,
	pub address: u16,
	pub bytes: Vec<u8>
}

impl Display for DisassembledInstruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let instr_fmt = match self.instruction {
			Some(ref instr) => format!("{}", instr),
			None => "???".to_string()
		};

		write!(f, "{:<20} ; [{:04x}]  ", instr_fmt,  self.address);

		for byte in &self.bytes {
			write!(f, "{:02x} ", byte);
		}

		Ok(())
	}
}

pub fn parse_instruction(memory: &Memory, addr: u16) -> (u16, DisassembledInstruction) {
	InstructionParser::new(memory, addr).parse_instruction()
}

struct InstructionParser<'a> {
	memory: &'a Memory,
	addr: u16,
	bytes: Vec<u8>
}

fn instr(mnemonic: Mnemonic, addressing: Addressing) -> Option<Instruction> {
	Some(Instruction(mnemonic, addressing))
}

impl<'a> InstructionParser<'a> {
	pub fn new(memory: &'a Memory, addr: u16) -> InstructionParser<'a> {
		InstructionParser {
			memory: memory,
			addr: addr,
			bytes: Vec::with_capacity(6)
		}
	}

	fn take_u8(&mut self) -> u8 {
		let value = self.memory.read_u8(self.addr);
		self.bytes.push(value);
		self.addr = self.addr.wrapping_add(1);

		value
	}

	fn take_u16(&mut self) -> u16 {
		let value = self.memory.read_u16(self.addr);
		
		let (hi, lo) = unpack_u16(value); 
		self.bytes.push(hi);
		self.bytes.push(lo);

		self.addr = self.addr.wrapping_add(2);

		value
	}

	fn parse_immediate8(&mut self) -> Addressing {
		Addressing::Immediate8(self.take_u8())
	}

	fn parse_immediate16(&mut self) -> Addressing {
		Addressing::Immediate16(self.take_u16())
	}

	fn parse_relative8(&mut self) -> Addressing {
		Addressing::Relative8(self.take_u8() as i8)
	}

	fn parse_relative16(&mut self) -> Addressing {
		Addressing::Relative16(self.take_u16() as i16)
	}

	fn parse_direct(&mut self) -> Addressing {
		Addressing::Direct(self.take_u8())
	}

	fn parse_extended(&mut self) -> Addressing {
		Addressing::Extended(self.take_u16())
	}

	fn parse_indexed(&mut self) -> Addressing {
		Addressing::Indexed(self.parse_index_mode())
	}

	fn parse_index_mode(&mut self) -> IndexMode {
		let postbyte = self.take_u8();

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
			2 => return IndexMode::Decrement1 { reg: reg },
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
				offset: self.take_u8() as i8,
				indirect: indirect
			},
			0b1001 => IndexMode::Offset16 {
				reg: reg,
				offset: self.take_u16() as i16,
				indirect: indirect
			},

			0b1100 => IndexMode::PcOffset8 {
				reg: reg,
				offset: self.take_u8() as i8,
				indirect: indirect
			},
			0b1101 => IndexMode::PcOffset16 {
				reg: reg,
				offset: self.take_u16() as i16,
				indirect: indirect
			},

			0b1111 => IndexMode::ExtendedIndirect {
				address: self.take_u16(),
			},

			_ => {
				panic!("Unknown index postbyte op: {:04b}", index_op);
			}
		}
	}

	pub fn parse_instruction(mut self) -> (u16, DisassembledInstruction) {
		let start_addr = self.addr;

		const PAGE_2: u8 = 0x10;
		const PAGE_3: u8 = 0x11;

		let op = self.take_u8();
		
		let instr = match op {
			PAGE_2 => {
				let op = self.take_u8();
				
				match op {
					0x83 => instr(Mnemonic::Cmpd, self.parse_immediate16()),
					0x93 => instr(Mnemonic::Cmpd, self.parse_direct()),
					0xa3 => instr(Mnemonic::Cmpd, self.parse_indexed()),
					0xb3 => instr(Mnemonic::Cmpd, self.parse_extended()),
					0x8c => instr(Mnemonic::Cmpy, self.parse_immediate16()),
					0x9c => instr(Mnemonic::Cmpy, self.parse_direct()),
					0xac => instr(Mnemonic::Cmpy, self.parse_indexed()),
					0xbc => instr(Mnemonic::Cmpy, self.parse_extended()),
					0xce => instr(Mnemonic::Lds, self.parse_immediate16()),
					0xde => instr(Mnemonic::Lds, self.parse_direct()),
					0xee => instr(Mnemonic::Lds, self.parse_indexed()),
					0xfe => instr(Mnemonic::Lds, self.parse_extended()),
					0x8e => instr(Mnemonic::Ldy, self.parse_immediate16()),
					0x9e => instr(Mnemonic::Ldy, self.parse_direct()),
					0xae => instr(Mnemonic::Ldy, self.parse_indexed()),
					0xbe => instr(Mnemonic::Ldy, self.parse_extended()),
					0x9f => instr(Mnemonic::Sty, self.parse_direct()),
					0xaf => instr(Mnemonic::Sty, self.parse_indexed()),
					0xbf => instr(Mnemonic::Sty, self.parse_extended()),
					0x3f => instr(Mnemonic::Swi2, Addressing::Inherent),
					0x27 => instr(Mnemonic::Beq, self.parse_relative16()),
					0x2c => instr(Mnemonic::Bge, self.parse_relative16()),
					0x2e => instr(Mnemonic::Bgt, self.parse_relative16()),
					0x22 => instr(Mnemonic::Bhi, self.parse_relative16()),
					0x24 => instr(Mnemonic::Bhs, self.parse_relative16()),
					0x2f => instr(Mnemonic::Ble, self.parse_relative16()),
					0x25 => instr(Mnemonic::Blo, self.parse_relative16()),
					0x23 => instr(Mnemonic::Bls, self.parse_relative16()),
					0x2d => instr(Mnemonic::Blt, self.parse_relative16()),
					0x2b => instr(Mnemonic::Bmi, self.parse_relative16()),
					0x26 => instr(Mnemonic::Bne, self.parse_relative16()),
					0x2a => instr(Mnemonic::Bpl, self.parse_relative16()),
					0x21 => instr(Mnemonic::Brn, self.parse_relative16()),
					0x28 => instr(Mnemonic::Bvc, self.parse_relative16()),
					0x29 => instr(Mnemonic::Bvs, self.parse_relative16()),

					_ => None
				}
			},
			PAGE_3 => {
				let op = self.take_u8();

				match op {
					0x8c => instr(Mnemonic::Cmps, self.parse_immediate16()),
					0x9c => instr(Mnemonic::Cmps, self.parse_direct()),
					0xac => instr(Mnemonic::Cmps, self.parse_indexed()),
					0xbc => instr(Mnemonic::Cmps, self.parse_extended()),
					0x83 => instr(Mnemonic::Cmpu, self.parse_immediate16()),
					0x93 => instr(Mnemonic::Cmpu, self.parse_direct()),
					0xa3 => instr(Mnemonic::Cmpu, self.parse_indexed()),
					0xb3 => instr(Mnemonic::Cmpu, self.parse_extended()),
					0x3f => instr(Mnemonic::Swi3, Addressing::Inherent),

					_ => None
				}
			},
			0x3a => instr(Mnemonic::Abx, Addressing::Inherent),
			0x89 => instr(Mnemonic::Adca, self.parse_immediate8()),
			0x99 => instr(Mnemonic::Adca, self.parse_direct()),
			0xa9 => instr(Mnemonic::Adca, self.parse_indexed()),
			0xb9 => instr(Mnemonic::Adca, self.parse_extended()),
			0xc9 => instr(Mnemonic::Adcb, self.parse_immediate8()),
			0xd9 => instr(Mnemonic::Adcb, self.parse_direct()),
			0xe9 => instr(Mnemonic::Adcb, self.parse_indexed()),
			0xf9 => instr(Mnemonic::Adcb, self.parse_extended()),
			0x8b => instr(Mnemonic::Adda, self.parse_immediate8()),
			0x9b => instr(Mnemonic::Adda, self.parse_direct()),
			0xab => instr(Mnemonic::Adda, self.parse_indexed()),
			0xbb => instr(Mnemonic::Adda, self.parse_extended()),
			0xcb => instr(Mnemonic::Addb, self.parse_immediate8()),
			0xdb => instr(Mnemonic::Addb, self.parse_direct()),
			0xeb => instr(Mnemonic::Addb, self.parse_indexed()),
			0xfb => instr(Mnemonic::Addb, self.parse_extended()),
			0xc3 => instr(Mnemonic::Addd, self.parse_immediate16()),
			0xd3 => instr(Mnemonic::Addd, self.parse_direct()),
			0xe3 => instr(Mnemonic::Addd, self.parse_indexed()),
			0xf3 => instr(Mnemonic::Addd, self.parse_extended()),
			0x84 => instr(Mnemonic::Anda, self.parse_immediate8()),
			0x94 => instr(Mnemonic::Anda, self.parse_direct()),
			0xa4 => instr(Mnemonic::Anda, self.parse_indexed()),
			0xb4 => instr(Mnemonic::Anda, self.parse_extended()),
			0xc4 => instr(Mnemonic::Andb, self.parse_immediate8()),
			0xd4 => instr(Mnemonic::Andb, self.parse_direct()),
			0xe4 => instr(Mnemonic::Andb, self.parse_indexed()),
			0xf4 => instr(Mnemonic::Andb, self.parse_extended()),
			0x1c => instr(Mnemonic::Andcc, self.parse_immediate8()),
			0x48 => instr(Mnemonic::Asla, Addressing::Inherent),
			0x58 => instr(Mnemonic::Aslb, Addressing::Inherent),
			0x08 => instr(Mnemonic::Asl, self.parse_direct()),
			0x68 => instr(Mnemonic::Asl, self.parse_indexed()),
			0x78 => instr(Mnemonic::Asl, self.parse_extended()),
			0x47 => instr(Mnemonic::Asra, Addressing::Inherent),
			0x57 => instr(Mnemonic::Asrb, Addressing::Inherent),
			0x07 => instr(Mnemonic::Asr, self.parse_direct()),
			0x67 => instr(Mnemonic::Asr, self.parse_indexed()),
			0x77 => instr(Mnemonic::Asr, self.parse_extended()),
			0x85 => instr(Mnemonic::Bita, self.parse_immediate8()),
			0x95 => instr(Mnemonic::Bita, self.parse_direct()),
			0xa5 => instr(Mnemonic::Bita, self.parse_indexed()),
			0xb5 => instr(Mnemonic::Bita, self.parse_extended()),
			0xc5 => instr(Mnemonic::Bitb, self.parse_immediate8()),
			0xd5 => instr(Mnemonic::Bitb, self.parse_direct()),
			0xe5 => instr(Mnemonic::Bitb, self.parse_indexed()),
			0xf5 => instr(Mnemonic::Bitb, self.parse_extended()),
			0x4f => instr(Mnemonic::Clra, Addressing::Inherent),
			0x5f => instr(Mnemonic::Clrb, Addressing::Inherent),
			0x0f => instr(Mnemonic::Clr, self.parse_direct()),
			0x6f => instr(Mnemonic::Clr, self.parse_indexed()),
			0x7f => instr(Mnemonic::Clr, self.parse_extended()),
			0x81 => instr(Mnemonic::Cmpa, self.parse_immediate8()),
			0x91 => instr(Mnemonic::Cmpa, self.parse_direct()),
			0xa1 => instr(Mnemonic::Cmpa, self.parse_indexed()),
			0xb1 => instr(Mnemonic::Cmpa, self.parse_extended()),
			0xc1 => instr(Mnemonic::Cmpb, self.parse_immediate8()),
			0xd1 => instr(Mnemonic::Cmpb, self.parse_direct()),
			0xe1 => instr(Mnemonic::Cmpb, self.parse_indexed()),
			0xf1 => instr(Mnemonic::Cmpb, self.parse_extended()),
			0x8c => instr(Mnemonic::Cmpx, self.parse_immediate16()),
			0x9c => instr(Mnemonic::Cmpx, self.parse_direct()),
			0xac => instr(Mnemonic::Cmpx, self.parse_indexed()),
			0xbc => instr(Mnemonic::Cmpx, self.parse_extended()),
			0x44 => instr(Mnemonic::Lsra, Addressing::Inherent),
			0x54 => instr(Mnemonic::Lsrb, Addressing::Inherent),
			0x04 => instr(Mnemonic::Lsr, self.parse_direct()),
			0x64 => instr(Mnemonic::Lsr, self.parse_indexed()),
			0x74 => instr(Mnemonic::Lsr, self.parse_extended()),
			0x3d => instr(Mnemonic::Mul, Addressing::Inherent),
			0x40 => instr(Mnemonic::Nega, Addressing::Inherent),
			0x50 => instr(Mnemonic::Negb, Addressing::Inherent),
			0x00 => instr(Mnemonic::Neg, self.parse_direct()),
			0x60 => instr(Mnemonic::Neg, self.parse_indexed()),
			0x70 => instr(Mnemonic::Neg, self.parse_extended()),
			0x12 => instr(Mnemonic::Nop, Addressing::Inherent),
			0x8a => instr(Mnemonic::Ora, self.parse_immediate8()),
			0x9a => instr(Mnemonic::Ora, self.parse_direct()),
			0xaa => instr(Mnemonic::Ora, self.parse_indexed()),
			0xba => instr(Mnemonic::Ora, self.parse_extended()),
			0xca => instr(Mnemonic::Orb, self.parse_immediate8()),
			0xda => instr(Mnemonic::Orb, self.parse_direct()),
			0xea => instr(Mnemonic::Orb, self.parse_indexed()),
			0xfa => instr(Mnemonic::Orb, self.parse_extended()),
			0x1a => instr(Mnemonic::Orcc, self.parse_immediate8()),
			0x34 => instr(Mnemonic::Pshs, self.parse_immediate8()),
			0x36 => instr(Mnemonic::Pshu, self.parse_immediate8()),
			0x35 => instr(Mnemonic::Puls, self.parse_immediate8()),
			0x37 => instr(Mnemonic::Pulu, self.parse_immediate8()),
			0x49 => instr(Mnemonic::Rola, Addressing::Inherent),
			0x59 => instr(Mnemonic::Rolb, Addressing::Inherent),
			0x09 => instr(Mnemonic::Rol, self.parse_direct()),
			0x69 => instr(Mnemonic::Rol, self.parse_indexed()),
			0x79 => instr(Mnemonic::Rol, self.parse_extended()),
			0x46 => instr(Mnemonic::Rora, Addressing::Inherent),
			0x56 => instr(Mnemonic::Rorb, Addressing::Inherent),
			0x06 => instr(Mnemonic::Ror, self.parse_direct()),
			0x66 => instr(Mnemonic::Ror, self.parse_indexed()),
			0x76 => instr(Mnemonic::Ror, self.parse_extended()),
			0x3b => instr(Mnemonic::Rti, Addressing::Inherent),
			0x39 => instr(Mnemonic::Rts, Addressing::Inherent),
			0x82 => instr(Mnemonic::Sbca, self.parse_immediate8()),
			0x92 => instr(Mnemonic::Sbca, self.parse_direct()),
			0xa2 => instr(Mnemonic::Sbca, self.parse_indexed()),
			0xb2 => instr(Mnemonic::Sbca, self.parse_extended()),
			0xc2 => instr(Mnemonic::Sbcb, self.parse_immediate8()),
			0xd2 => instr(Mnemonic::Sbcb, self.parse_direct()),
			0xe2 => instr(Mnemonic::Sbcb, self.parse_indexed()),
			0xf2 => instr(Mnemonic::Sbcb, self.parse_extended()),
			0x1d => instr(Mnemonic::Sex, Addressing::Inherent),
			0x43 => instr(Mnemonic::Coma, Addressing::Inherent),
			0x53 => instr(Mnemonic::Comb, Addressing::Inherent),
			0x03 => instr(Mnemonic::Com, self.parse_direct()),
			0x63 => instr(Mnemonic::Com, self.parse_indexed()),
			0x73 => instr(Mnemonic::Com, self.parse_extended()),
			0x3c => instr(Mnemonic::Cwai, self.parse_immediate8()),
			0x19 => instr(Mnemonic::Daa, Addressing::Inherent),
			0x4a => instr(Mnemonic::Deca, Addressing::Inherent),
			0x5a => instr(Mnemonic::Decb, Addressing::Inherent),
			0x0a => instr(Mnemonic::Dec, self.parse_direct()),
			0x6a => instr(Mnemonic::Dec, self.parse_indexed()),
			0x7a => instr(Mnemonic::Dec, self.parse_extended()),
			0x88 => instr(Mnemonic::Eora, self.parse_immediate8()),
			0x98 => instr(Mnemonic::Eora, self.parse_direct()),
			0xa8 => instr(Mnemonic::Eora, self.parse_indexed()),
			0xb8 => instr(Mnemonic::Eora, self.parse_extended()),
			0xc8 => instr(Mnemonic::Eorb, self.parse_immediate8()),
			0xd8 => instr(Mnemonic::Eorb, self.parse_direct()),
			0xe8 => instr(Mnemonic::Eorb, self.parse_indexed()),
			0xf8 => instr(Mnemonic::Eorb, self.parse_extended()),
			0x1e => instr(Mnemonic::Exg, self.parse_immediate8()),
			0x4c => instr(Mnemonic::Inca, Addressing::Inherent),
			0x5c => instr(Mnemonic::Incb, Addressing::Inherent),
			0x0c => instr(Mnemonic::Inc, self.parse_direct()),
			0x6c => instr(Mnemonic::Inc, self.parse_indexed()),
			0x7c => instr(Mnemonic::Inc, self.parse_extended()),
			0x0e => instr(Mnemonic::Jmp, self.parse_direct()),
			0x6e => instr(Mnemonic::Jmp, self.parse_indexed()),
			0x7e => instr(Mnemonic::Jmp, self.parse_extended()),
			0x9d => instr(Mnemonic::Jsr, self.parse_direct()),
			0xad => instr(Mnemonic::Jsr, self.parse_indexed()),
			0xbd => instr(Mnemonic::Jsr, self.parse_extended()),
			0x86 => instr(Mnemonic::Lda, self.parse_immediate8()),
			0x96 => instr(Mnemonic::Lda, self.parse_direct()),
			0xa6 => instr(Mnemonic::Lda, self.parse_indexed()),
			0xb6 => instr(Mnemonic::Lda, self.parse_extended()),
			0xc6 => instr(Mnemonic::Ldb, self.parse_immediate8()),
			0xd6 => instr(Mnemonic::Ldb, self.parse_direct()),
			0xe6 => instr(Mnemonic::Ldb, self.parse_indexed()),
			0xf6 => instr(Mnemonic::Ldb, self.parse_extended()),
			0xcc => instr(Mnemonic::Ldd, self.parse_immediate16()),
			0xdc => instr(Mnemonic::Ldd, self.parse_direct()),
			0xec => instr(Mnemonic::Ldd, self.parse_indexed()),
			0xfc => instr(Mnemonic::Ldd, self.parse_extended()),
			0xce => instr(Mnemonic::Ldu, self.parse_immediate16()),
			0xde => instr(Mnemonic::Ldu, self.parse_direct()),
			0xee => instr(Mnemonic::Ldu, self.parse_indexed()),
			0xfe => instr(Mnemonic::Ldu, self.parse_extended()),
			0x8e => instr(Mnemonic::Ldx, self.parse_immediate16()),
			0x9e => instr(Mnemonic::Ldx, self.parse_direct()),
			0xae => instr(Mnemonic::Ldx, self.parse_indexed()),
			0xbe => instr(Mnemonic::Ldx, self.parse_extended()),
			0x32 => instr(Mnemonic::Leas, self.parse_indexed()),
			0x33 => instr(Mnemonic::Leau, self.parse_indexed()),
			0x30 => instr(Mnemonic::Leax, self.parse_indexed()),
			0x31 => instr(Mnemonic::Leay, self.parse_indexed()),
			0x97 => instr(Mnemonic::Sta, self.parse_direct()),
			0xa7 => instr(Mnemonic::Sta, self.parse_indexed()),
			0xb7 => instr(Mnemonic::Sta, self.parse_extended()),
			0xd7 => instr(Mnemonic::Stb, self.parse_direct()),
			0xe7 => instr(Mnemonic::Stb, self.parse_indexed()),
			0xf7 => instr(Mnemonic::Stb, self.parse_extended()),
			0xdd => instr(Mnemonic::Std, self.parse_direct()),
			0xed => instr(Mnemonic::Std, self.parse_indexed()),
			0xfd => instr(Mnemonic::Std, self.parse_extended()),
			0xdf => instr(Mnemonic::Stu, self.parse_direct()),
			0xef => instr(Mnemonic::Stu, self.parse_indexed()),
			0xff => instr(Mnemonic::Stu, self.parse_extended()),
			0x9f => instr(Mnemonic::Stx, self.parse_direct()),
			0xaf => instr(Mnemonic::Stx, self.parse_indexed()),
			0xbf => instr(Mnemonic::Stx, self.parse_extended()),
			0x80 => instr(Mnemonic::Suba, self.parse_immediate8()),
			0x90 => instr(Mnemonic::Suba, self.parse_direct()),
			0xa0 => instr(Mnemonic::Suba, self.parse_indexed()),
			0xb0 => instr(Mnemonic::Suba, self.parse_extended()),
			0xc0 => instr(Mnemonic::Subb, self.parse_immediate8()),
			0xd0 => instr(Mnemonic::Subb, self.parse_direct()),
			0xe0 => instr(Mnemonic::Subb, self.parse_indexed()),
			0xf0 => instr(Mnemonic::Subb, self.parse_extended()),
			0x83 => instr(Mnemonic::Subd, self.parse_immediate16()),
			0x93 => instr(Mnemonic::Subd, self.parse_direct()),
			0xa3 => instr(Mnemonic::Subd, self.parse_indexed()),
			0xb3 => instr(Mnemonic::Subd, self.parse_extended()),
			0x3f => instr(Mnemonic::Swi, Addressing::Inherent),
			0x13 => instr(Mnemonic::Sync, Addressing::Inherent),
			0x1f => instr(Mnemonic::Tfr, self.parse_immediate8()),
			0x4d => instr(Mnemonic::Tsta, Addressing::Inherent),
			0x5d => instr(Mnemonic::Tstb, Addressing::Inherent),
			0x0d => instr(Mnemonic::Tst, self.parse_direct()),
			0x6d => instr(Mnemonic::Tst, self.parse_indexed()),
			0x7d => instr(Mnemonic::Tst, self.parse_extended()),
			0x27 => instr(Mnemonic::Beq, self.parse_relative8()),
			0x2c => instr(Mnemonic::Bge, self.parse_relative8()),
			0x2e => instr(Mnemonic::Bgt, self.parse_relative8()),
			0x22 => instr(Mnemonic::Bhi, self.parse_relative8()),
			0x24 => instr(Mnemonic::Bhs, self.parse_relative8()),
			0x2f => instr(Mnemonic::Ble, self.parse_relative8()),
			0x25 => instr(Mnemonic::Blo, self.parse_relative8()),
			0x23 => instr(Mnemonic::Bls, self.parse_relative8()),
			0x2d => instr(Mnemonic::Blt, self.parse_relative8()),
			0x2b => instr(Mnemonic::Bmi, self.parse_relative8()),
			0x26 => instr(Mnemonic::Bne, self.parse_relative8()),
			0x2a => instr(Mnemonic::Bpl, self.parse_relative8()),
			0x20 => instr(Mnemonic::Bra, self.parse_relative8()),
			0x16 => instr(Mnemonic::Bra, self.parse_relative16()),
			0x21 => instr(Mnemonic::Brn, self.parse_relative8()),
			0x8d => instr(Mnemonic::Bsr, self.parse_relative8()),
			0x17 => instr(Mnemonic::Bsr, self.parse_relative16()),
			0x28 => instr(Mnemonic::Bvc, self.parse_relative8()),
			0x29 => instr(Mnemonic::Bvs, self.parse_relative8()),

			_ => None
		};

		let InstructionParser { bytes, memory: _, addr } = self;

		let instr = DisassembledInstruction {
			instruction: instr,
			address: start_addr,
			bytes: bytes
		};

		(addr, instr)
	}
}