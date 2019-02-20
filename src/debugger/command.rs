use std::{fmt, u16};

#[derive(Clone, Debug)]
pub enum Register { A, B, DP, CC, D, X, Y, U, S, PC }

impl Register {
	pub fn parse(input: &str) -> Option<Register> {
		Some(match input.to_lowercase().as_ref() {
			"a" => Register::A,
			"b" => Register::B,
			"d" => Register::D,
			"x" => Register::X,
			"y" => Register::Y,
			"u" => Register::U,
			"s" => Register::S,
			"dp" => Register::DP,
			"pc" => Register::PC,
			"cc" => Register::CC,
			_ => return None
		})
	}
}

pub enum AddressRef {
	Literal(u16),
	Register(Register),
	Label(String)
}

impl fmt::Display for AddressRef {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&AddressRef::Literal(addr) => write!(f, "{:04x}", addr),
			&AddressRef::Register(ref reg) => write!(f, "[{:?}]", reg),
			&AddressRef::Label(ref label) => write!(f, ":{}", label)
		}
	}
}

impl AddressRef {
	pub fn parse(addr: &str) -> Option<AddressRef> {
		let addr = addr.trim();

		Register::parse(addr).map(|r| AddressRef::Register(r))
			.or_else(||
				u16::from_str_radix(addr, 16)
					.map(|a| AddressRef::Literal(a)).ok()
			)
			.or_else(||
				if addr.starts_with(":") {
					let mut addr = addr.to_string();
					addr.remove(0);
					Some(AddressRef::Label(addr))
				}
				else { None }
			)
	}
}

pub enum Command {
	Quit,
	Continue,
	DisplayRegisters,
	Step { count: usize },
	ToggleTrace,
	ToggleBreakpoints,
	Disassemble {
		length: u16,
		address: AddressRef
	},
	AddBreakpoint { address: AddressRef },
	DeleteBreakpoint { address: AddressRef },
	ListBreakpoints,
	ClearBreakpoints,
	SetLabel {
		label: String,
		address: AddressRef
	},
	RemoveLabel { label: String },
	ShowLabel { label: String },
	ListLabels,
	ClearLabels,
	ViewMemory {
		address: AddressRef,
		length: u16
	}
}

#[derive(Debug)]
pub enum ParseError {
	Empty,
	InvalidCommand(String),
	InvalidArgument(&'static str),
	MissingArgument(&'static str),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    	match self {
    		&ParseError::Empty => Ok(()),
    		&ParseError::InvalidCommand(ref cmd) => write!(f, "{}: Invalid command", cmd),
    		&ParseError::InvalidArgument(ref argname) => write!(f, "Invalid argument '{}'", argname),
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

		fn parse_address(arg: Option<&str>) -> Result<AddressRef, ParseError> {
			arg.ok_or(ParseError::MissingArgument("address"))
				.and_then(|arg|
					AddressRef::parse(arg)
						.ok_or(ParseError::InvalidArgument("address"))
				)
		}

		match command {
			"q" | "quit" | "exit" | ":wq" => Ok(Command::Quit),
			"s" | "step" => {
				let count = args.next()
					.and_then(|a| usize::from_str_radix(a, 10).ok())
					.unwrap_or(1);

				Ok(Command::Step { count: count })
			},
			"r" | "reg" | "registers" => Ok(Command::DisplayRegisters),
			"c" | "cnt" | "continue" => Ok(Command::Continue),
			"t" | "trace" => Ok(Command::ToggleTrace),
			"d" | "dis" | "disassemble" => {
				parse_address(args.next().or(Some("pc")))
					.map(|addr| {
						let length = args.next()
							.and_then(|len| len.parse::<u16>().ok())
							.unwrap_or(30);
						
						Command::Disassemble {
							length: length,
							address: addr
						}
					})
			},
			"l" | "label" | "labels" => {
				Ok(match args.next() {
					Some(arg) => match arg {
						"ls" => Command::ListLabels,
						"rm" => match args.next() {
							Some(label) => Command::RemoveLabel { label: label.to_string() },
							None => return Err(ParseError::MissingArgument("label"))
						},
						"clear" | "clr" => Command::ClearLabels,
						label_arg => match args.next() {
							Some(addr_arg) => return parse_address(Some(addr_arg))
								.map(|addr| Command::SetLabel {
									label: label_arg.to_string(),
									address: addr
								}),
							None => Command::ShowLabel { label: label_arg.to_string() }
						}
					},
					None => Command::ListLabels
				})
			}
			"b" | "break" | "breakpoint" => {
				let arg1 = match args.next() {
					Some(arg) => arg,
					None => return Ok(Command::ListBreakpoints)
				};

				let (is_delete, addr) = match arg1 {
					"ls" => return Ok(Command::ListBreakpoints),
					"rm" => (true, args.next().and_then(AddressRef::parse)),
					"t" | "toggle" => return Ok(Command::ToggleBreakpoints),
					"clr" | "clear" => return Ok(Command::ClearBreakpoints),
					addr => (false, AddressRef::parse(addr))
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
			"m" | "mem" | "memory" => {
				parse_address(args.next())
					.map(|address| {
						let length = args.next()
							.and_then(|len| len.parse::<u16>().ok())
							.unwrap_or(16);

						Command::ViewMemory {
							address: address,
							length: length
						}
					})
			}
			_ => Err(ParseError::InvalidCommand(command.to_string()))
		}
	}
}