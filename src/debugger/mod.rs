#![allow(dead_code)]

pub mod command;
pub mod label_registry;
pub mod user_data;

use self::command::*;
use self::label_registry::*;
use self::user_data::*;

use std::thread;
use std::sync::mpsc::Receiver;
use std::collections::HashSet;
use std::time::{SystemTime, Duration};

use buffering_line_sink::BufferingLineSink;
use minifb_line_sink::MinifbDriver;
use line_sink::FrameSink;
use mc6809::Mc6809;
use memory::Memory;
use vectrex::Vectrex;
use disassembler;

const FRAME_RATE: usize = 30;
const CYCLES_PER_SECOND: usize = 15_000_000;
const CYCLES_PER_FRAME: usize = CYCLES_PER_SECOND / FRAME_RATE;
static FRAME_TIME_NS: u32 = 1_000_000_000 / 30;

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
	breakpoints_enabled: bool,
	breakpoints: HashSet<u16>,
	labels: LabelRegistry,

	buffer_sink: BufferingLineSink,
	minifb_sink: MinifbDriver,

	frame_cycles: i32,
	last_frame: SystemTime
}

fn print_label(label: &str, address: u16) {
	println!("{:04x} :{}", address, label);
}

fn format_address(address: &AddressRef, resolved_address: u16) -> String {
	match address {
		&AddressRef::Literal(address) => format!("{:04x}", address),
		address => format!("{} ({:04x})", address, resolved_address)
	}
}

impl Debugger {
	pub fn new(vectrex: Vectrex, command_receiver: Receiver<String>, user_data: Option<UserData>) -> Debugger {
		let user_data = user_data.unwrap_or_default();

		Debugger {
			vectrex: vectrex,
			command_receiver: command_receiver,
			state: State::Debugging,
			trace_enabled: false,
			breakpoints_enabled: true,
			breakpoints: user_data.breakpoints,
			labels: user_data.labels,
			buffer_sink: Default::default(),
			minifb_sink: MinifbDriver::new(),
			frame_cycles: CYCLES_PER_FRAME as i32,
			last_frame: SystemTime::now()
		}
	}

	fn should_break_on(&self, address: u16) -> bool {
		self.breakpoints_enabled && self.breakpoints.contains(&address)
	}

	fn step(&mut self) {
		if self.trace_enabled {
			let pc = self.cpu().reg_pc();
			let (_, instr) = disassembler::parse_instruction(self.mem(), pc);
			println!("{}", instr);
		}
		let num_cycles = self.vectrex.step(&mut self.buffer_sink);
		self.frame_cycles -= num_cycles as i32;
		if self.frame_cycles < 0 {
			let dt = self.last_frame.elapsed().unwrap();
			if let Some(sleep_duration) = Duration::new(0, FRAME_TIME_NS).checked_sub(dt) {
				thread::sleep(sleep_duration);
			}
			
			let lines = self.buffer_sink.collect();
			(&mut self.minifb_sink as &mut FrameSink).append(lines);
			self.frame_cycles += CYCLES_PER_FRAME as i32;
		}

		let pc = self.cpu().reg_pc();
		if self.should_break_on(pc) {
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

	pub fn resolve_address(&self, addr: &AddressRef) -> Option<u16> {
		let cpu = self.vectrex.cpu();
		match addr {
			&AddressRef::Literal(v) => Some(v),
			&AddressRef::Register(ref r) => Some(match r {
				&Register::X => cpu.reg_x(),
				&Register::Y => cpu.reg_y(),
				&Register::U => cpu.reg_u(),
				&Register::S => cpu.reg_s(),
				&Register::PC => cpu.reg_pc(),
				_ => return None
			}),
			&AddressRef::Label(ref label) => self.labels.address_for_label(label)
		}
	}

	fn process_command_queue(&mut self) {
		let mut should_save = false;

		while let Ok(cmd) = self.command_receiver.try_recv() {
			let cmd = match Command::parse(cmd) {
				Ok(cmd) => {
					// Save on every valid command, works well enough for now
					should_save = true;
					cmd
				},
				Err(command::ParseError::Empty) => continue,
				Err(err) => {
					error!("{}", err);
					continue
				}
			};

			macro_rules! try_resolve_address {
				($address:expr) => ({
					match self.resolve_address($address) {
						Some(a) => a,
						None => {
							println!("Unable to resolve address: {}", $address);
							continue
						}
					}
				})
			}

			match cmd {
				Command::DisplayRegisters => println!("{}", self.cpu()),
				Command::Continue => {
					self.state = State::Running;
				},
				Command::Step { count } => {
					self.state = State::Running;

					for _ in 0..count {
						if self.state != State::Running {
							break
						}

						self.step();
					}

					self.state = State::Debugging;
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
				Command::Disassemble { length, ref address } => {
					let mut pc = try_resolve_address!(address);

					for _ in 0..length {
						let (next_pc, instr) = disassembler::parse_instruction(self.mem(), pc);
						let breakpoint = match self.breakpoints.contains(&pc) {
							true if self.breakpoints_enabled => "*",
							true => "'",
							false => " "
						};

						println!("{} {}", breakpoint, instr);
						pc = next_pc;
					}
				},
				Command::ToggleBreakpoints => {
					self.breakpoints_enabled = !self.breakpoints_enabled;
					println!("Breakpoints {}", match self.breakpoints_enabled {
						true => "enabled",
						false => "disabled"
					});
				}
				Command::AddBreakpoint { ref address } => {
					let resolved_address = try_resolve_address!(address);
					match self.breakpoints.insert(resolved_address) {
						true => println!("Added breakpoint at {}", format_address(address, resolved_address)),
						false => println!("Breakpoint already exists at {}", format_address(address, resolved_address))
					}
				},
				Command::DeleteBreakpoint { ref address } => {
					let resolved_address = try_resolve_address!(address);

					match self.breakpoints.remove(&resolved_address) {
						true => println!("Removed breakpoint at {}", format_address(address, resolved_address)),
						false => println!("No breakpoint at {}", format_address(address, resolved_address))
					}
				},
				Command::ClearBreakpoints => {
					self.breakpoints.clear();
					println!("All breakpoints cleared");
				}
				Command::ListBreakpoints => {
					println!("Currently active breakpoints:");
					for addr in &self.breakpoints {
						let labels_str = self.labels.map.iter()
							.filter_map(|pair| {
								let (label, label_addr) = pair;
								if addr == label_addr {
									Some(label.as_str())
								}
								else { None }
							})
							.collect::<Vec<_>>()
							.join(", ");

						print!("{:04x}", addr);

						if !labels_str.is_empty() {
							print!(" ({})", labels_str);
						}

						println!("");
					}
				},
				Command::ListLabels => {
					for (label, addr) in self.labels.map.iter() {
						print_label(label, *addr);
					}
				},
				Command::ShowLabel { ref label } => {
					match self.labels.address_for_label(label) {
						Some(address) => print_label(label, address),
						None => println!("Unknown label '{}'", label)
					}
				}
				Command::SetLabel { ref label, ref address } => {
					let address = try_resolve_address!(address);
					match self.labels.set_label(label.to_string(), address) {
						Some(old_addr) => println!(":{} = {:04x} (was {:04x})", label, address, old_addr),
						None => println!(":{} = {:04x}", label, address)
					}

				},
				Command::RemoveLabel { ref label } => {
					match self.labels.remove_label(label) {
						Some(addr) => println!("Removed label '{}' ({:04x})", label, addr),
						None => println!("Unknown label '{}'", label)
					}
				},
				Command::ViewMemory { address, length } => {
					let address = try_resolve_address!(&address);

					const BYTES_PER_ROW: u8 = 8;

					let mut bytes = Vec::with_capacity(length as _);
					for i in 0..length {
						let byte = self.mem().read_8(address.wrapping_add(i));
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

		if should_save {
			if let Err(err) = self.user_data().save() {
				warn!("Unable to save user data: {}", err);
			}
		}
	}
}