#[macro_use]
extern crate log;
extern crate env_logger;

mod cartridge;
mod bios;
mod sram;
mod mem_map;
mod memory;
mod motherboard;
mod disassembler;
mod mc6809;
mod vectrex;
mod pack;
mod debugger;

use bios::Bios;
use cartridge::Cartridge;
use vectrex::Vectrex;
use debugger::Debugger;

use std::thread;
use std::sync::mpsc;
use std::io::stdin;

fn main() {
	env_logger::init().unwrap();

	let bios = include_bytes!("../roms/bios.vec");
	let bios = Bios::from_bytes(bios).unwrap();

	let cart = include_bytes!("../roms/PolePosition.vec");
	let cart = Cartridge::from_bytes(cart).ok();

	let vectrex = Vectrex::new(bios, cart);

	let (cmd_sender, cmd_reciever) = mpsc::channel();

	let debugger = Debugger::new(vectrex, cmd_reciever);

	let _ = thread::spawn(move || {
		loop {
			let mut line = String::new();
			stdin().read_line(&mut line).unwrap();
			line = line.trim().into();

			cmd_sender.send(line).unwrap()
		}
	});

	debugger.run();
}
