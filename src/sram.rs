use mem_map::{SRAM_LENGTH, SRAM_REAL_SIZE};

pub struct Sram {
	bytes: [u8; SRAM_REAL_SIZE as usize]
}

impl Sram {
	pub fn new() -> Sram {
		Sram {
			bytes: [0; SRAM_REAL_SIZE as usize]
		}
	}

	pub fn read_8(&self, addr: u16) -> u8 {
		self.bytes[(addr % SRAM_REAL_SIZE) as usize]
	}

	pub fn write_8(&mut self, addr: u16, value: u8) {
		self.bytes[(addr % SRAM_REAL_SIZE) as usize] = value;
	}
}