#[macro_use]
extern crate log;
extern crate env_logger;

mod cartridge;
mod bios;
mod mem_map;
mod motherboard;
mod instruction;
mod mc6809;
mod vectrex;

use bios::Bios;
use cartridge::Cartridge;
use vectrex::Vectrex;
use instruction::Opcode;

fn main() {
	env_logger::init().unwrap();

	let bios = include_bytes!("../roms/minestorm.vec");
	let bios = Bios::from_bytes(bios).unwrap();

	let cart = include_bytes!("../roms/PolePosition.vec");
	let cart = Cartridge::from_bytes(cart).ok();

	let mut vectrex = Vectrex::new(bios, cart);

	for _ in 0..20 {
		{
			let op_u16 = vectrex.motherboard().read_u16(vectrex.cpu().reg_pc());
			let opcode = Opcode::from_u16(op_u16);
			println!("] {:?}", opcode);
		}
		vectrex.step();
	}
}
