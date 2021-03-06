use pack::*;
use super::instruction::*;
use std::fmt::{ self, Display, Formatter };

impl Display for Mnemonic {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		// Uppercase the result of derived Debug for now
		let mnemonic = format!("{:?}", self).to_uppercase();
		write!(f, "{}", mnemonic)
	}
}

impl Display for Register {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

fn fmt_indirect<C>(f: &mut Formatter, indirect: bool, callback: C) -> fmt::Result
	where C : Fn(&mut Formatter) -> fmt::Result {

	if indirect {
		write!(f, "[")?;
		callback(f)?;
		write!(f, "]")?;
	}
	else {
		callback(f)?;
	}

	Ok(())
}

impl Display for IndexMode {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			&IndexMode::Offset0 {ref reg, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, ", {}", reg)
			}),

			&IndexMode::Offset5 {ref reg, ref offset} => write!(f, "${:02x}, {}", offset, reg),

			&IndexMode::Offset8 {ref reg, ref offset, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, "${:02x}, {}", offset, reg)
			}),

			&IndexMode::Offset16 {ref reg, ref offset, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, "${:04x}, {}", offset, reg)
			}),

			&IndexMode::OffsetA {ref reg, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, "A, {}", reg)
			}),

			&IndexMode::OffsetB {ref reg, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, "B, {}", reg)
			}),

			&IndexMode::OffsetD {ref reg, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, "D, {}", reg)
			}),

			&IndexMode::Increment1 {ref reg} => write!(f, ", {}+", reg),
			&IndexMode::Decrement1 {ref reg} => write!(f, ", {}-", reg),

			&IndexMode::Increment2 {ref reg, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, ", {}++", reg)
			}),

			&IndexMode::Decrement2 {ref reg, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, ", {}--", reg)
			}),

			&IndexMode::PcOffset8 {ref offset, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, "{:02x}, PCR", offset)
			}),

			&IndexMode::PcOffset16 {ref offset, ref indirect} => fmt_indirect(f, *indirect, |f| {
				write!(f, "{:04x}, PCR", offset)
			}),

			&IndexMode::ExtendedIndirect {ref address} => write!(f, "[{:04x}]", address)
		}
	}
}

impl Display for Addressing {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			&Addressing::Inherent => Ok(()),
			&Addressing::Immediate8(value)  => write!(f, "#${:02X}", value),
			&Addressing::Immediate16(value) => write!(f, "#${:04X}", value),
			&Addressing::Relative8(offset)  => write!(f, "${:02X}", offset),
			&Addressing::Relative16(offset) => write!(f, "${:04X}", offset),
			&Addressing::Direct(addr_lo)    => write!(f, "< ${:02X}", addr_lo),
			&Addressing::Extended(addr)     => write!(f, "${:04X}", addr),
			&Addressing::Indexed(ref index_mode) => write!(f, "{}", index_mode)
		}
	}
}

fn fmt_tfr_exg_nibble(nibble: u8) -> String {
	match Register::from_tfr_exg_nibble(nibble) {
		Some(reg) => format!("{}", reg),
		None => "???".to_string()
	}
}

enum StackPtrReg { S, U }

fn fmt_psh_pul_postbyte(postbyte: u8, stack_ptr_reg: StackPtrReg) -> String {
	let result = String::new();
	let regs = unpack_flags(postbyte);

	regs.iter()
		.enumerate()
		.filter_map(|e| {
			let (index, enabled) = e;
			if !enabled { return None }
			Some(match index {
				0 => "CC",
				1 => "A",
				2 => "B",
				3 => "DP",
				4 => "X",
				5 => "Y",
				6 => match stack_ptr_reg {
					StackPtrReg::S => "S",
					StackPtrReg::U => "U"
				},
				7 => "PC",
				_ => return None
			})
		})
		.collect::<Vec<_>>()
		.join(",")
}

impl Display for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let &Instruction(ref mnemonic, _) = self;

		let addressing_fmt = match self {
			&Instruction(Mnemonic::Tfr, Addressing::Immediate8(postbyte)) |
			&Instruction(Mnemonic::Exg, Addressing::Immediate8(postbyte)) => {
				let (reg_a, reg_b) = unpack_nibbles(postbyte);
				format!("{},{}", fmt_tfr_exg_nibble(reg_a), fmt_tfr_exg_nibble(reg_b))
			},
			&Instruction(Mnemonic::Pshu, Addressing::Immediate8(postbyte)) |
			&Instruction(Mnemonic::Pulu, Addressing::Immediate8(postbyte)) => {
				fmt_psh_pul_postbyte(postbyte, StackPtrReg::U)
			},
			&Instruction(Mnemonic::Pshs, Addressing::Immediate8(postbyte)) |
			&Instruction(Mnemonic::Puls, Addressing::Immediate8(postbyte)) => {
				fmt_psh_pul_postbyte(postbyte, StackPtrReg::S)
			},
			&Instruction(_, ref addressing) => {
				format!("{}", addressing)
			}
		};

		let mnemonic = format!("{}", mnemonic);
		write!(f, "{:<6} {}", mnemonic, addressing_fmt)
	}
}