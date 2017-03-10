#![allow(dead_code)]

mod command;

use self::command::Command;

use std::sync::mpsc::Receiver;
use std::collections::HashSet;

use mc6809::Mc6809;
use memory::Memory;
use vectrex::Vectrex;
use disassembler;

#[derive(PartialEq)]
pub enum State {
	Running,
	Debugging,
	Quitting
}

pub struct Debugger {
	vectrex: Vectrex,
	command_receiver: Receiver<String>,
	state: State,
	trace_enabled: bool,
	breakpoints: HashSet<u16>
}

impl Debugger {
	pub fn new(vectrex: Vectrex, command_receiver: Receiver<String>) -> Debugger {
		Debugger {
			vectrex: vectrex,
			command_receiver: command_receiver,
			state: State::Debugging,
			trace_enabled: false,
			breakpoints: HashSet::new()
		}
	}

	fn process_command_queue(&mut self) {
		while let Ok(cmd) = self.command_receiver.try_recv() {
			let cmd = match Command::parse(cmd) {
				Ok(cmd) => cmd,
				Err(command::ParseError::Empty) => continue,
				Err(err) => {
					error!("{}", err);
					continue
				}
			};

			match cmd {
				Command::DisplayRegisters => println!("{}", self.cpu()),
				Command::Continue => {
					self.state = State::Running;
				},
				Command::Step => {
					self.step();
				},
				Command::ToggleTrace => {
					self.trace_enabled = !self.trace_enabled;
					println!("Trace {}", match self.trace_enabled {
						true => "enabled",
						false => "disabled"
					});
				},
				Command::Quit => {
					self.state = State::Quitting;
				},
				Command::Disassemble { length, address } => {
					let mut pc = address.unwrap_or(self.cpu().reg_pc());

					for _ in 0..length {
						let (next_pc, instr) = disassembler::parse_instruction(self.mem(), pc);
						println!("{}", instr);
						pc = next_pc;
					}
				},
				Command::AddBreakpoint { address } => {
					match self.breakpoints.insert(address) {
						true => println!("Added breakpoint at {:04x}", address),
						false => println!("Breakpoint already exists at {:04x}", address)
					}
				},
				Command::DeleteBreakpoint { ref address } => {
					match self.breakpoints.remove(address) {
						true => println!("Removed breakpoint at {:04x}", address),
						false => println!("No breakpoint at {:04x}", address)
					}
				},
				Command::ListBreakpoints => {
					println!("Currently active breakpoints:");
					for addr in &self.breakpoints {
						println!("{:04x}", addr);
					}
				},
				Command::ViewMemory { address, length } => {
					const BYTES_PER_ROW: u8 = 8;

					let mut bytes = Vec::with_capacity(length as _);
					for i in 0..length {
						let byte = self.mem().read_u8(address.wrapping_add(i));
						bytes.push(byte);
					}

					let hexdump = bytes
						.chunks(BYTES_PER_ROW as usize)
						.map(|chunk| chunk
							.iter()
							.map(|b| format!("{:02x}", b))
							.collect::<Vec<_>>()
							.join(" "))
						.collect::<Vec<_>>()
						.join("\n");

					println!("{}", hexdump);
				}
			};

			println!("");
		}
	}

	fn step(&mut self) {
		if self.trace_enabled {
			let pc = self.cpu().reg_pc();
			let (_, instr) = disassembler::parse_instruction(self.mem(), pc);
			println!("{}", instr);
		}

		self.vectrex.step();

		let pc = self.cpu().reg_pc();
		if self.breakpoints.contains(&pc) {
			println!("Hit breakpoint at {:04x}", pc);
			println!("");
			self.state = State::Debugging;
		}
	}

	pub fn run(mut self) {
		loop {
			match self.state {
				State::Quitting => break,
				State::Debugging => self.process_command_queue(),
				State::Running => self.step()
			}
		}
	}

	fn cpu(&self) -> &Mc6809 { self.vectrex.cpu() }
	fn mem(&mut self) -> &mut Memory { self.vectrex.motherboard() }
}