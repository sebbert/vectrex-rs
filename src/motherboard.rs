use mem_map::*;
use bios::Bios;
use cartridge::Cartridge;
use sram::Sram;

pub struct Motherboard {
	bios: Bios,
	cartridge: Option<Cartridge>,
	sram: Sram,
}

impl Motherboard {
	pub fn new(bios: Bios, cartridge: Option<Cartridge>) -> Motherboard {
		Motherboard {
			bios: bios,
			cartridge: cartridge,
			sram: Sram::new(),
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
			SRAM_START ... SRAM_END => {
				self.sram.read_u8(addr - SRAM_START)
			}
			_ => panic!("Read from unmapped address: 0x{:04x}", addr)
		}
	}

	pub fn read_u16(&self, addr: u16) -> u16 {
		let hi = self.read_u8(addr) as u16;
		let lo = self.read_u8(addr+1) as u16;

		(hi << 8) | lo
	}
	
	pub fn write_u8(&mut self, addr: u16, value: u8) {
		match addr {
			CARTRIDGE_START ... CARTRIDGE_END => {
				warn!("Attempted write to cartridge ROM");
			}
			BIOS_START ... BIOS_END => {
				warn!("Attempted write to BIOS");
			}
			SRAM_START ... SRAM_END => {
				self.sram.write_u8(addr - SRAM_START, value)
			}
			_ => panic!("Write to unmapped address: 0x{:04x}", addr)
		}
	}

	pub fn write_u16(&mut self, addr: u16, value: u16) {
		let hi = (value >> 8) as u8;
		let lo = value as u8;

		self.write_u8(addr, hi);
		self.write_u8(addr+1, lo);
	}
}