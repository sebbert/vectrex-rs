#![allow(unused_imports)]

use motherboard::Motherboard;
use debugger::disassembler::parse_instruction;
use debugger::instruction::{IndexMode, Register};
use mem_map::*;
use bios::Bios;

#[test]
fn display_index_mode_offset_0() {
	let index_mode = IndexMode::Offset0 {
		reg: Register::U,
		indirect: true
	};

	let display = format!("{}", index_mode);

	assert_eq!(display, "[, U]");
}