#[macro_use]
extern crate log;
extern crate env_logger;

mod rom;
mod mem_map;
mod motherboard;
mod instruction;
mod mc6809;
mod vectrex;

use rom::Rom;
use vectrex::Vectrex;
use instruction::Opcode;

use std::env;
use std::io::prelude::*;
use std::fs::File;

static bios: &'static [u8] = include_bytes!("../roms/minestorm.vec");

fn main() {
	env_logger::init().unwrap();

	let mut vectrex = Vectrex::new(Rom::from_bytes(bios), None);

	for _ in 0..80 {
		{

			let op_u16 = vectrex.motherboard().read_u16(vectrex.cpu().reg_pc());
			let opcode = Opcode::from_u16(op_u16);
			println!("] {:?}", opcode);
		}
		vectrex.step();
	}
}
