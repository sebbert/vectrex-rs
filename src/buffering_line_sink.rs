#![allow(unused)]


use line_sink::*;

#[derive(Default)]
pub struct BufferingLineSink {
	lines: Vec<Line>
}

impl LineSink for BufferingLineSink {
	fn append(&mut self, line: Line) {
		self.lines.push(line);
	}
}

impl FrameSink for BufferingLineSink {
	fn append(&mut self, mut lines: Vec<Line>) {
		self.lines.append(&mut lines);
	}
}

impl BufferingLineSink {
	pub fn collect(&mut self) -> Vec<Line> {
		self.lines.drain(..).collect()
	}
}