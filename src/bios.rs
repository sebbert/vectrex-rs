use mem_map::BIOS_LENGTH;

pub struct Bios {
	#[allow(dead_code)] bytes: Box<[u8]>,
	bytes_ptr: *mut u8
}

#[derive(Debug)]
pub enum Error {
	InvalidBiosSize
}

impl Bios {
	pub fn from_bytes(bytes: &[u8]) -> Result<Bios, Error> {
		if bytes.len() != BIOS_LENGTH as usize {
			return Err(Error::InvalidBiosSize)
		}

		let mut bytes = bytes.to_vec().into_boxed_slice();
		let bytes_ptr = bytes.as_mut_ptr();

		Ok(Bios {
			bytes: bytes,
			bytes_ptr: bytes_ptr
		})
	}

	pub fn read_u8(&self, addr: u16) -> u8 {
		let addr = addr & 0x7fff;
		unsafe {
			*self.bytes_ptr.offset(addr as _)
		}
	}
}