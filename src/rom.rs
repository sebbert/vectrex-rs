pub struct Rom {
	bytes: Box<[u8]>,
	bytes_ptr: *mut u8
}

impl Rom {
	pub fn from_bytes(bytes: &[u8]) -> Rom {
		let mut bytes = bytes.to_vec().into_boxed_slice();
		let bytes_ptr = bytes.as_mut_ptr();

		Rom {
			bytes: bytes,
			bytes_ptr: bytes_ptr
		}
	}

	pub fn read_u8(&self, addr: u16) -> u8 {
		unsafe {
			*self.bytes_ptr.offset(addr as _)
		}
	}

	pub fn read_u16(&self, addr: u16) -> u16 {
		unsafe {
			let hi = *self.bytes_ptr.offset(addr as _) as u16;
			let lo = *self.bytes_ptr.offset((addr + 1) as _) as u16;

			(lo << 8) | hi
		}
	}
}