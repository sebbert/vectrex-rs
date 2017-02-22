use mem_map::SRAM_LENGTH;

pub struct Sram {
	bytes: [u8; SRAM_LENGTH as usize]
}

impl Sram {
	pub fn new() -> Sram {
		Sram {
			bytes: [0; SRAM_LENGTH as usize]
		}
	}

	pub fn read_u8(&self, addr: u16) -> u8 {
		self.bytes[addr as usize]
	}

	pub fn write_u8(&mut self, addr: u16, value: u8) {
		self.bytes[addr as usize] = value;
	}
}