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
					let pc = self.cpu().reg_pc();
					let (_, instr) = disassembler::parse_instruction(self.mem(), pc);
					println!("{}", instr);
					self.vectrex.step();
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
				}
			};

			println!("");
		}
	}

	pub fn run(mut self) {
		loop {
			match self.state {
				State::Quitting => break,
				State::Debugging => self.process_command_queue(),
				State::Running => self.vectrex.step()
			}
		}
	}

	fn cpu(&self) -> &Mc6809 { self.vectrex.cpu() }
	fn mem(&self) -> &Memory { self.vectrex.motherboard() }
}