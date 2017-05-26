use mem_map::CARTRIDGE_LENGTH;

pub struct Cartridge {
	#[allow(dead_code)] bytes: Box<[u8]>,
	bytes_ptr: *const u8
}

pub enum Error {
	InvalidCartridgeSize
}

impl Cartridge {
	pub fn from_bytes(bytes: &[u8]) -> Result<Cartridge, Error> {
		if bytes.len() > CARTRIDGE_LENGTH as usize {
			return Err(Error::InvalidCartridgeSize)
		}

		let bytes = bytes.to_vec().into_boxed_slice();
		let bytes_ptr = bytes.as_ptr();

		Ok(Cartridge {
			bytes: bytes,
			bytes_ptr: bytes_ptr
		})
	}

	pub fn read_8(&self, addr: u16) -> u8 {
		let addr = addr & 0x7fff;
		unsafe {
			*self.bytes_ptr.offset(addr as _)
		}
	}
}