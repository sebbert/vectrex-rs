#![allow(dead_code)]

pub const CARTRIDGE_START: u16 = 0x0;
pub const CARTRIDGE_END: u16 = 0x7fff;
pub const CARTRIDGE_LENGTH: u16 = CARTRIDGE_END - CARTRIDGE_START + 1;

pub const SRAM_START :u16 = 0xc800;
pub const SRAM_END :u16 = 0xcbff;
pub const SRAM_LENGTH: u16 = SRAM_END - SRAM_START + 1;

pub const MINE_STORM_START: u16 = 0xe000;
pub const MINE_STORM_END: u16 = 0xefff;
pub const MINE_STORM_LENGTH: u16 = MINE_STORM_END - MINE_STORM_START + 1;

pub const EXECUTIVE_START: u16 = 0xf000;
pub const EXECUTIVE_END: u16 = 0xffff;
pub const EXECUTIVE_LENGTH: u16 = EXECUTIVE_END - EXECUTIVE_START + 1;

pub const BIOS_START: u16 = MINE_STORM_START;
pub const BIOS_END: u16 = EXECUTIVE_END;
pub const BIOS_LENGTH: u16 = BIOS_END - BIOS_START + 1;