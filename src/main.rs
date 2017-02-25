#[macro_use]
extern crate log;
extern crate env_logger;

mod cartridge;
mod bios;
mod sram;
mod mem_map;
mod memory;
mod motherboard;
mod debugger;
mod mc6809;
mod vectrex;
mod pack;

use bios::Bios;
use cartridge::Cartridge;
use vectrex::Vectrex;
use debugger::Opcode;
use memory::Memory;

fn main() {
	env_logger::init().unwrap();

	let bios = include_bytes!("../roms/bios.vec");
	let bios = Bios::from_bytes(bios).unwrap();

	let cart = include_bytes!("../roms/PolePosition.vec");
	let cart = Cartridge::from_bytes(cart).ok();

	let mut vectrex = Vectrex::new(bios, cart);

	for _ in 0..20 {
		let _ = vectrex.step();

		{
			let cpu = vectrex.cpu();
			let pc = cpu.reg_pc();
			let op_u16 = vectrex.motherboard().read_u16(pc);
			let opcode = Opcode::from_u16(op_u16);
			println!("[0x{:04x}] {:?}", pc, opcode);
			println!();
			println!("{:?}", cpu);
		}
	}
}
