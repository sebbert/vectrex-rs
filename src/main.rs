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

use std::env;
use std::io::prelude::*;
use std::fs::File;

static bios: &'static [u8] = include_bytes!("../roms/minestorm.vec");

fn main() {
	env_logger::init().unwrap();

	let mut vectrex = Vectrex::new(Rom::from_bytes(bios), None);

	for _ in 0..80 {
		vectrex.step();
	}
}
