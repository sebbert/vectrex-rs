pub struct Line {
	pub start: (i8, i8),
	pub end: (i8, i8),
	pub brightness: u8
}

pub trait LineSink {
	fn append(&mut self, line: Line);
}