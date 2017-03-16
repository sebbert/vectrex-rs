use line_sink::{Line, LineSink};

pub struct StupidDebugLineSink {}

impl LineSink for StupidDebugLineSink {
	fn append(&mut self, line: Line) {
		let (x0, y0) = line.start;
		let (x1, y1) = line.end;
		println!("Draw line from ({}, {}) to ({}, {})", x0, y0, x1, y1);
	}
}