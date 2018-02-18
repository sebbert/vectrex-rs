use mem_map::SRAM_LENGTH;

pub struct Sram {
	bytes: [u8; SRAM_LENGTH as usize]
}

impl Sram {
	pub fn new() -> Sram {
		let mut bytes = [0u8; SRAM_LENGTH as usize];
		for i in 0..bytes.len() {
			bytes[i] = i as u8 & 0xff;
		}

		Sram { bytes }
	}

	pub fn read_8(&self, addr: u16) -> u8 {
		self.bytes[addr as usize]
	}

	pub fn write_8(&mut self, addr: u16, value: u8) {
		self.bytes[addr as usize] = value;
	}
}