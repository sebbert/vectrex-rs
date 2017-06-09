use line_sink::*;

pub struct BufferingLineSink {
	lines: Vec<Line>
}

impl LineSink for BufferingLineSink {
	fn append(&mut self, line: Line) {
		self.lines.push(line);
	}
}

impl BufferingLineSink {
	pub fn collect(&mut self) -> Vec<Line> {
		self.lines.drain(..).collect()
	}
}