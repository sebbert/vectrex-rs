pub trait Memory {
	fn read_8(&mut self, addr: u16) -> u8;
	fn write_8(&mut self, addr: u16, value: u8);
	
	fn read_16(&mut self, addr: u16) -> u16;
	fn write_16(&mut self, addr: u16, value: u16);
}