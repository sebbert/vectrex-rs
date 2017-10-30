use vec2::*;

pub struct Line {
	pub start: Vec2,
	pub end: Vec2,
	pub brightness: u8
}

pub trait LineSink {
	fn append(&mut self, line: Line);
}

pub trait FrameSink {
	fn append(&mut self, Vec<Line>);
}