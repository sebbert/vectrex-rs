use debugger::instruction::*;
use std::fmt::{ self, Display, Formatter };

impl Display for Mnemonic {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		// Uppercase the result of derived Debug for now
		let mnemonic = format!("{:?}", self).to_uppercase();

		write!(f, "{}", mnemonic)

	}
}

impl Display for IndexRegister {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

fn fmt_indirect<C>(f: &mut Formatter, indirect: bool, callback: C) -> fmt::Result
	where C : Fn(&mut Formatter) -> fmt::Result {

	if indirect {
		try!(write!(f, "["));
		try!(callback(f));
		try!(write!(f, "]"));
	}
	else {
		try!(callback(f));
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

impl Display for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let &Instruction(ref mnemonic, ref addressing) = self;

		let mnemonic = format!("{}", mnemonic);

		write!(f, "{:<6} {}", mnemonic, addressing)
	}
}