use motherboard::Motherboard;
use rom::Rom;
use mc6809::Mc6809;

pub struct Vectrex {
	motherboard: Motherboard,
	cpu: Mc6809
}

impl Vectrex {
	pub fn new(bios: Rom, cartridge: Option<Rom>) -> Vectrex {
		let mobo = Motherboard::new(bios, None);
		let mut cpu = Mc6809::new();
		cpu.reset(&mobo);

		Vectrex {
			motherboard: mobo,
			cpu: cpu
		}
	}

	pub fn step(&mut self) {
		self.cpu.step(&mut self.motherboard);
	}

	pub fn cpu(&self) -> &Mc6809 {
		&self.cpu
	}

	pub fn motherboard(&self) -> &Motherboard {
		&self.motherboard
	}
}