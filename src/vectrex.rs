use motherboard::Motherboard;
use bios::Bios;
use cartridge::Cartridge;
use mc6809::Mc6809;
use line_sink::LineSink;

pub struct Vectrex {
	motherboard: Motherboard,
	cpu: Mc6809
}

impl Vectrex {
	pub fn new(bios: Bios, cartridge: Option<Cartridge>) -> Vectrex {
		let mut mobo = Motherboard::new(bios, cartridge);
		let cpu = Mc6809::new(&mut mobo);

		Vectrex {
			motherboard: mobo,
			cpu: cpu
		}
	}

	pub fn step(&mut self, line_sink: &mut LineSink) {
		let cycles = self.cpu.step(&mut self.motherboard, false);
		self.motherboard.step_for(cycles, line_sink);
	}

	pub fn cpu(&self) -> &Mc6809 {
		&self.cpu
	}

	pub fn motherboard(&mut self) -> &mut Motherboard {
		&mut self.motherboard
	}
}