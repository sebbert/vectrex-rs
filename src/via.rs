pub struct Via {
	
}

impl Via {
	pub fn new() -> Via {
		Via {}
	}

	pub fn read(&self, addr: u16) -> u8 {
		let addr = Self::mask_addr(addr);
		match addr {
			_ => {
				warn!("VIA {:01x}", addr);
				0 
			}
		}
	}

	pub fn write(&self, addr: u16, value: u8) {
		let addr = Self::mask_addr(addr);
		match addr {
			_ => {
				warn!("VIA {:01x} = {:02x}", addr, value);
			}
		}
	}

	fn mask_addr(addr: u16) -> u16 {
		addr & 0x000f
	}
}