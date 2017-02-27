#![allow(dead_code)]

mod command;

use self::command::Command;

use std::sync::mpsc::Receiver;

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
	state: State
}

impl Debugger {
	pub fn new(vectrex: Vectrex, command_receiver: Receiver<String>) -> Debugger {
		Debugger {
			vectrex: vectrex,
			command_receiver: command_receiver,
			state: State::Debugging
		}
	}

	pub fn step(&mut self) {
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
				Command::Continue => {},
				Command::Quit => {
					self.state = State::Quitting;
					return
				},
				Command::Disassemble { length, address } => {
					let mut pc = address.unwrap_or(self.cpu().reg_pc());

					for _ in 0..length {
						let (next_pc, instr) = disassembler::parse_instruction(self.mem(), pc);
						println!("{}", instr);
						pc = next_pc;

					}
				}
			};
		}
	}

	pub fn run(mut self) {
		let mut keep_running = true;
		while keep_running {
			match self.state {
				State::Quitting => keep_running = false,
				State::Debugging => self.step(),
				_ => {}
			}
		}
	}

	fn cpu(&self) -> &Mc6809 { self.vectrex.cpu() }
	fn mem(&self) -> &Memory { self.vectrex.motherboard() }
}