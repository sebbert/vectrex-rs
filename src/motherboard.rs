use mem_map::*;
use rom::Rom;

pub struct Motherboard {
	bios: Rom,
	cartridge: Option<Rom>
}

impl Motherboard {
	pub fn new(bios: Rom, cartridge: Option<Rom>) -> Motherboard {
		Motherboard {
			bios: bios,
			cartridge: cartridge
		}
	}

	pub fn read_u8(&self, addr: u16) -> u8 {
		match addr {
			CARTRIDGE_START ... CARTRIDGE_END => {
				match self.cartridge {
					Some(ref cartridge) => cartridge.read_u8(addr - CARTRIDGE_START),
					None => {
						warn!("Read from non-existent cartridge (at 0x{:04x})", addr);
						0
					}
				}
			}
			BIOS_START ... BIOS_END => {
				self.bios.read_u8(addr - BIOS_START)
			}
			_ => panic!("Read from unmapped address: 0x{:04x}", addr)
		}
	}

	pub fn read_u16(&self, addr: u16) -> u16 {
		// Do two u8 reads for now, but should probably do the matching here as well
		let hi = self.read_u8(addr) as u16;
		let lo = self.read_u8(addr + 1) as u16;

		(hi << 8) | lo
	}

	pub fn write_u8(&self, addr: u16, value: u8) {
		match addr {
			CARTRIDGE_START ... CARTRIDGE_END => {
				warn!("Attempted write to cartridge ROM");
			}
			BIOS_START ... BIOS_END => {
				warn!("Attempted write to BIOS");
			}
			_ => panic!("Write to unmapped address: 0x{:04x}", addr)
		}
	}

	pub fn write_u16(&self, addr: u16, value: u16) {
		match addr {
			_ => panic!("Write to unmapped address: 0x{:04x}", addr)
		}
	}
}