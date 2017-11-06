extern crate minifb;

use minifb::{Window, WindowOptions};

use line_sink::{Line, LineSink, FrameSink};
use vec2::*;

const WIDTH: usize = 500;
const HEIGHT: usize = 600;
const PIXEL_COUNT: usize = WIDTH * HEIGHT;

pub struct MinifbDriver {
	window: Window,
	buffer: [u32; PIXEL_COUNT]
}

impl MinifbDriver {
	pub fn new() -> MinifbDriver {
		let window = Window::new(
			"vectrex-rs",
			WIDTH,
			HEIGHT,
			WindowOptions::default()).unwrap();

		MinifbDriver {
			window,
			buffer: [0; PIXEL_COUNT]
		}
	}

	pub fn clear(&mut self) {
		for i in self.buffer.iter_mut() {
			*i = 0;
		}
	}
}

impl FrameSink for MinifbDriver {
	fn append(&mut self, lines: Vec<Line>) {
		self.clear();
		for line in lines {
			(self as &mut LineSink).append(line);
		}
		self.window.update_with_buffer(&self.buffer);
	}
}

impl LineSink for MinifbDriver {
	fn append(&mut self, line: Line) {
		let (width, height) = (WIDTH as i32, HEIGHT as i32);
		let size = Vec2::new(width, height);
		let half = size / 2;
		let line = Line {
			start: (line.start * 8) / width + half,
			end: (line.end * 8) / width + half,
			brightness: line.brightness
		};

		let difference = line.end - line.start;
		let mut d = 2*difference.y - difference.x;
		let mut y = line.start.y;

		let buf_len = (&self.buffer).len();
		for x in line.start.x .. (line.end.x+1) {
			self.buffer[vbuf_index(x as usize, y as usize).min(buf_len-1)] = !0;
			if d >= 0 {
				y += 1;
				d -= 2 * difference.x;
			}
			d += 2 * difference.y;
		}
	}
}

fn vbuf_index(x: usize, y: usize) -> usize {
	let x = x.max(0).min(WIDTH);
	let y = y.max(0).min(HEIGHT);
	x + (y * WIDTH)
}