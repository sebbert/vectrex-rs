use debugger::instruction::*;
use std::fmt::{ self, Display, Formatter };

impl Display for Mnemonic {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		// Uppercase the result of derived Debug for now
		let mnemonic = format!("{:?}", self).to_uppercase();

		write!(f, "{}", mnemonic)

	}
}

impl Display for Instruction {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		let &Instruction(ref mnemonic, ref addressing) = self;

		write!(f, "{}", mnemonic)
	}
}