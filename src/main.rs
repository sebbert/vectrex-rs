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

use bios::Bios;
use cartridge::Cartridge;
use vectrex::Vectrex;
use disassembler::parse_instruction;

fn main() {
	env_logger::init().unwrap();

	let bios = include_bytes!("../roms/bios.vec");
	let bios = Bios::from_bytes(bios).unwrap();

	let cart = include_bytes!("../roms/PolePosition.vec");
	let cart = Cartridge::from_bytes(cart).ok();

	let mut vectrex = Vectrex::new(bios, cart);

	for _ in 0..20 {
		{
			let cpu = vectrex.cpu();
			let pc = cpu.reg_pc();

			let (_, instr) = parse_instruction(vectrex.motherboard(), pc);

			println!("{}", instr);

			println!("{:?}", cpu);
		}

		let _ = vectrex.step();
	}
}
