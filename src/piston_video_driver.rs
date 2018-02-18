use piston_window::*;
use line_sink::{LineSink, Line as VectrexLine};

pub struct PistonVideoDriver {
	window: PistonWindow,
	lines: Vec<VectrexLine>
}

impl PistonVideoDriver {
	pub fn new(window: Window) -> PistonVideoDriver {
		PistonVideoDriver {
			window: window,
			lines: Vec::new()
		}
	}

	pub fn update(&mut self) {
		self.window.draw_2d(&e, |ctx, gfx| {
			clear([0.0, 0.0, 0.0, 1.0], gfx);
			
			let window_size = self.window.draw_size();

			for (i, &vec_line) in self.lines.iter().enumerate() {
				vec_line.brightness = vec_line.brightness.saturating_sub(2);
				if vec_line.brightness == 0 {
					self.lines.remove(i);
				}

				let (x0, y0) = scale_coord(vec_line.start, window_size);
				let (x1, y1) = scale_coord(vec_line.end, window_size);

				line([1.0; 4], 2., [x0, y0, x1, y1], ctx.transform, gfx);
			}
		});
	}
}

fn scale_coord(vec_coord: (i8, i8), size: Size) -> (f64, f64) {
	let (x, y) = vec_coord;
	let x = x as f64 / 128.;
	let y = y as f64 / 128.;

	let width = size.width as f64;
	let height = size.height as f64;

	(x * width, y * height)
}

impl LineSink for PistonVideoDriver {
	fn append(&mut self, line: VectrexLine) {
		self.lines.push(line);
	}
}