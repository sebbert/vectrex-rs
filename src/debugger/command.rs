use std::fmt;

pub enum Command {
	Quit,
	Continue,
	DisplayRegisters,
	Step,
	Disassemble {
		length: u16,
		address: Option<u16>
	}
}

#[derive(Debug)]
pub enum ParseError {
	Empty,
	InvalidCommand(String)
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		&ParseError::Empty => Ok(()),
    		&ParseError::InvalidCommand(ref cmd) => write!(f, "{}: Invalid command", cmd),
    	}
    }
}

impl Command {
	pub fn parse(input: String) -> Result<Command, ParseError> {
		let mut args = input.split_whitespace();

		let command = match args.next() {
			Some(cmd) => cmd,
			None => return Err(ParseError::Empty)
		};

		match command {
			"q" | "quit" | "exit" | ":wq" => Ok(Command::Quit),
			"s" | "step" => Ok(Command::Step),
			"r" | "reg" | "registers" => Ok(Command::DisplayRegisters),
			"c" | "cnt" | "continue" => Ok(Command::Continue),
			"d" | "dis" | "disassemble" => {
				let address = args.next()
					.and_then(|a| if a == "pc" { None } else { Some(a) })
					.and_then(|addr| addr.parse::<u16>().ok());

				let length = args.next()
					.and_then(|len| len.parse::<u16>().ok())
					.unwrap_or(30);
				
				Ok(Command::Disassemble {
					length: length,
					address: address
				})
			}
			_ => Err(ParseError::InvalidCommand(command.to_string()))
		}
	}
}