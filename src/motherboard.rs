use mem_map::*;
use bios::Bios;
use cartridge::Cartridge;
use sram::Sram;
use memory::Memory;
use via::Via;
use line_sink::LineSink;
use pack::*;

pub struct Motherboard {
	bios: Bios,
	cartridge: Option<Cartridge>,
	sram: Sram,
	via: Via
}

impl Motherboard {
	pub fn new(bios: Bios, cartridge: Option<Cartridge>) -> Motherboard {
		Motherboard {
			bios: bios,
			cartridge: cartridge,
			sram: Sram::new(),
			via: Via::new()
		}
	}
	pub fn step_for(&mut self, cycles: usize, line_sink: &mut LineSink) {
		for _ in 0..cycles {
			self.via.step(line_sink);
		}
	}
}

impl Memory for Motherboard {
	fn read_8(&mut self, addr: u16) -> u8 {
		match addr {
			CARTRIDGE_START ... CARTRIDGE_END => {
				match self.cartridge {
					Some(ref cartridge) => cartridge.read_8(addr - CARTRIDGE_START),
					None => {
						warn!("Read from non-existent cartridge (at 0x{:04x})", addr);
						0
					}
				}
			}
			BIOS_START ... BIOS_END => {
				self.bios.read_8(addr - BIOS_START)
			}
			SRAM_START ... SRAM_END => {
				self.sram.read_8(addr - SRAM_START)
			}
			VIA_START ... VIA_END => {
				self.via.read(addr)
			}
			_ => panic!("Read from unmapped address: 0x{:04x}", addr)
		}
	}
	
	fn write_8(&mut self, addr: u16, value: u8) {
		match addr {
			CARTRIDGE_START ... CARTRIDGE_END => {
				warn!("Attempted write to cartridge ROM");
			}
			BIOS_START ... BIOS_END => {
				warn!("Attempted write to BIOS");
			}
			SRAM_START ... SRAM_END => {
				self.sram.write_8(addr - SRAM_START, value)
			}
			VIA_START ... VIA_END => {
				self.via.write(addr, value)
			}
			_ => panic!("Write to unmapped address: 0x{:04x}", addr)
		}
	}

	fn read_16(&mut self, addr: u16) -> u16 {
		let hi = self.read_8(addr);
		let lo = self.read_8(addr + 1);

		pack_16(hi, lo)
	}

	fn write_16(&mut self, addr: u16, value: u16) {
		let (hi, lo) = unpack_16(value);

		self.write_8(addr, hi);
		self.write_8(addr + 1, lo);
	}
}