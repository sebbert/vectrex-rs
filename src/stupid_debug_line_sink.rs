use line_sink::{Line, LineSink};
use vec2::*;

pub struct StupidDebugLineSink {}

impl LineSink for StupidDebugLineSink {
	fn append(&mut self, line: Line) {
		let Vec2 { x: x0, y: y0 } = line.start;
		let Vec2 { x: x1, y: y1 } = line.end;
		debug!("Draw line from ({}, {}) to ({}, {})", x0, y0, x1, y1);
	}
}