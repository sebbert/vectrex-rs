use motherboard::Motherboard;
use bios::Bios;
use cartridge::Cartridge;
use mc6809::Mc6809;

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

	pub fn step(&mut self) {
		self.cpu.step(&mut self.motherboard);
	}

	pub fn cpu(&self) -> &Mc6809 {
		&self.cpu
	}

	pub fn motherboard(&mut self) -> &mut Motherboard {
		&mut self.motherboard
	}
}