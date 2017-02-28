#![allow(unused_imports)]

use super::instruction::{IndexMode, Register};

#[test]
fn display_index_mode_offset_0() {
	let index_mode = IndexMode::Offset0 {
		reg: Register::U,
		indirect: true
	};

	let display = format!("{}", index_mode);

	assert_eq!(display, "[, U]");
}