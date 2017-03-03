use std::{fmt, u16};

pub enum Command {
	Quit,
	Continue,
	DisplayRegisters,
	Step,
	ToggleTrace,
	Disassemble {
		length: u16,
		address: Option<u16>
	},
	AddBreakpoint { address: u16 },
	DeleteBreakpoint { address: u16 },
	ListBreakpoints
}

#[derive(Debug)]
pub enum ParseError {
	Empty,
	InvalidCommand(String),
	MissingArgument(&'static str)
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		&ParseError::Empty => Ok(()),
    		&ParseError::InvalidCommand(ref cmd) => write!(f, "{}: Invalid command", cmd),
    		&ParseError::MissingArgument(ref argname) => write!(f, "Missing argument '{}'", argname),
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
			"t" | "trace" => Ok(Command::ToggleTrace),
			"d" | "dis" | "disassemble" => {

				let address = parse_address(args.next());

				let length = args.next()
					.and_then(|len| len.parse::<u16>().ok())
					.unwrap_or(30);
				
				Ok(Command::Disassemble {
					length: length,
					address: address
				})
			},
			"b" | "break" | "breakpoint" => {
				let arg1 = match args.next() {
					Some(arg) => arg,
					None => return Ok(Command::ListBreakpoints)
				};

				let (is_delete, addr) = match arg1 {
					"list" | "show" => return Ok(Command::ListBreakpoints),
					"del" | "delete" => (true, parse_address(args.next())),
					addr => (false, parse_address(Some(addr)))
				};

				let addr = match addr {
					Some(addr) => addr,
					None => return Err(ParseError::MissingArgument("address"))
				};

				Ok(match is_delete {
					true => Command::DeleteBreakpoint { address: addr },
					false => Command::AddBreakpoint { address: addr }
				})
			},
			_ => Err(ParseError::InvalidCommand(command.to_string()))
		}
	}
}

fn parse_address(addr: Option<&str>) -> Option<u16> {
	addr.map(|a| a.trim())
		.and_then(|a| if a == "pc" { None } else { Some(a) })
		.and_then(|a| u16::from_str_radix(a, 16).ok())
}