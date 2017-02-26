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
			_ => write!(f, "????")
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