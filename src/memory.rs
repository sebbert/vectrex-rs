use pack::*;

pub trait Memory {
	fn read_u8(&self, addr: u16) -> u8;
	fn write_u8(&mut self, addr: u16, value: u8);
	
	fn read_u16(&self, addr: u16) -> u16;
	fn write_u16(&mut self, addr: u16, value: u16);
}