#![allow(dead_code)]

use pack::*;
use line_sink::*;
use vec2::*;

const REG_PORT_B: u16 = 0x0;
const REG_PORT_A: u16 = 0x1;
const REG_PORT_A_NO_HANDSHAKE: u16 = 0xf;
const REG_DDRB: u16 = 0x2;
const REG_DDRA: u16 = 0x3;
const REG_T1_COUNTER_LO: u16 = 0x4;
const REG_T1_COUNTER_HI: u16 = 0x5;
const REG_T1_LATCH_LO: u16 = 0x6;
const REG_T1_LATCH_HI: u16 = 0x7;
const REG_T2_LO: u16 = 0x8;
const REG_T2_HI: u16 = 0x9;
const REG_SHIFT: u16 = 0xa;
const REG_ACR: u16 = 0xb;
const REG_PCR: u16 = 0xc;
const REG_IFR: u16 = 0xd;
const REG_IER: u16 = 0xe;

const ORB_FLAG_MUX_ENABLED: usize = 0;
const ORB_FLAG_MUX_SEL0: usize = 1;
const ORB_FLAG_MUX_SEL1: usize = 2;
const ORB_FLAG_AY_BC1: usize = 3;
const ORB_FLAG_AY_BDIR: usize = 4;
const ORB_FLAG_CARTRIDGE_SOMETHING: usize = 6;
const ORB_FLAG_ZERO_BEAM: usize = 7;

enum MuxChannel {
	YAxis,
	XYAxis,
	ZAxis,
	Sound
}

impl Default for MuxChannel {
	fn default() -> MuxChannel { MuxChannel::YAxis }
}

#[derive(Default)]
pub struct Via {
	ier_t1: bool,
	ifr_t1: bool,

	acr_t1_continuous: bool,
	acr_t1_pb7: bool,

	t1_counter_lo: u8,
	t1_counter_hi: u8,
	t1_latch_lo: u8,
	t1_latch_hi: u8,
	t1_pb7: bool,
	t1_running: bool,

	ora: u8,
	orb: u8,
	
	pcr_ca: ControlLineConfig,
	pcr_cb: ControlLineConfig,
	
	pulse_state: bool,

	// X, Y and Z sample-and-hold
	x_sh: i8,
	y_sh: i8,
	z_sh: i8,
	
	beam_dx: i8,
	beam_dy: i8,
	
	beam_brightness: u8,
	beam_prev_position: Vec2,
	beam_current_position: Vec2
}

impl Via {
	pub fn new() -> Via {
		Default::default()	
	}

	pub fn step(&mut self, line_sink: &mut LineSink) -> bool {
		let next_t1_counter = self.t1_counter().wrapping_sub(1);
		self.set_t1_counter(next_t1_counter);
		
		if next_t1_counter == 0xffff {
			self.ifr_t1 = true;
			self.t1_pb7 = !self.t1_pb7;
			let latch = self.t1_latch();
			self.set_t1_counter(latch);

			if !self.acr_t1_continuous {
				self.t1_running = !self.t1_running;
			}
		}
		self.pulse_state = false;
		self.hardware_step(line_sink);
		self.pulse_state = true;
		
		self.ier() & self.ifr() != 0
	}
	
	fn hardware_step(&mut self, line_sink: &mut LineSink) {
		self.beam_prev_position = self.beam_current_position;
		
		self.beam_current_position = if self.beam_should_zero() {
		 	Vec2::default()
		}
		else if self.beam_should_integrate() {
			self.beam_current_position + Vec2 {
				x: self.ora as i32,
				y: self.y_sh as i32
			}
		}
		else { self.beam_current_position };
		
		if self.beam_enabled() {
			line_sink.append(Line {
				start: self.beam_prev_position,
				end: self.beam_current_position,
				brightness: 255
			})
		}
	}

	pub fn read(&mut self, addr: u16) -> u8 {
		let addr = Self::mask_addr(addr);
		match addr {
			REG_T1_COUNTER_LO => {
				self.ifr_t1 = false;
				self.t1_counter_lo
			}
			REG_T1_COUNTER_HI => self.t1_counter_hi,
			REG_T1_LATCH_LO => self.t1_latch_lo,
			REG_T1_LATCH_HI => self.t1_latch_hi,
			REG_IER => self.ier(),
			REG_IFR => self.ifr(),
			REG_PCR => self.pcr(),
			REG_PORT_B => self.orb,
			REG_PORT_A => self.ora,
			_ => {
				error!("Read from unimplemented VIA reg {:01x}", addr);
				0
			}
		}
	}

	pub fn write(&mut self, addr: u16, value: u8) {
		let addr = Self::mask_addr(addr);
		match addr {
			REG_T1_COUNTER_LO | REG_T1_LATCH_LO => {
				self.t1_latch_lo = value;
			},
			REG_T1_LATCH_HI => {
				self.t1_latch_hi = value;
			},
			REG_T1_COUNTER_HI => {
				self.t1_latch_hi = value;
				self.t1_counter_hi = self.t1_latch_hi;
				self.t1_counter_lo = self.t1_latch_lo;
				self.ifr_t1 = false;
				self.t1_pb7 = false;
			},
			REG_IER => self.set_ier(value),
			REG_IFR => self.set_ifr(value),
			REG_PCR => self.set_pcr(value),
			REG_PORT_A => self.set_ora(value),
			REG_PORT_B => self.set_orb(value),
			REG_PORT_A_NO_HANDSHAKE => self.set_ora(value),

			_ => warn!("Write to unimplemented VIA reg {:01x} = {:02x}", addr, value)
		}
	}
	
	#[allow(unused_variables)]
	fn control_line_2(&self, pcr: &ControlLineConfig) -> bool {
		use self::ControlLine2Config::*;
		match pcr.c2_config {
			Input { active_edge, independent_interrupt } => {
				debug!("Control line 2 read while in input mode");
				false
			},
			Output(value) => value,
			HandshakeOutput(_) => {
				error!("VIA Handshaking not implemented");
				false
			}
			PulseOutput => self.pulse_state
		}
	}
	
	fn ca2(&self) -> bool {
		self.control_line_2(&self.pcr_ca)
	}
	
	fn cb2(&self) -> bool {
		// TODO: Shift reg
		self.control_line_2(&self.pcr_cb)
	}
	
	fn beam_should_zero(&self) -> bool { !self.ca2() }
	
	fn beam_enabled(&self) -> bool { !self.cb2() }

	fn beam_should_integrate(&self) -> bool { !self.port_b_bit_7_out() }
	
	fn pcr(&self) -> u8 {
		ControlLineConfig::encode_cb_ca(&self.pcr_cb, &self.pcr_ca)
	}
	
	fn set_pcr(&mut self, pcr: u8) {
		let (cb, ca) = ControlLineConfig::parse_cb_ca(pcr);
		self.pcr_cb = cb;
		self.pcr_ca = ca;
	}

	fn mask_addr(addr: u16) -> u16 {
		addr & 0xf
	}

	fn t1_counter(&self) -> u16 {
		pack_u16(self.t1_counter_hi, self.t1_counter_lo)
	}

	fn t1_latch(&self) -> u16 {
		pack_u16(self.t1_latch_hi, self.t1_latch_lo)
	}

	fn set_t1_counter(&mut self, value: u16) {
		let (hi, lo) = unpack_u16(value);
		self.t1_counter_hi = hi;
		self.t1_counter_lo = lo;
	}

	fn set_t1_latch(&mut self, value: u16) {
		let (hi, lo) = unpack_u16(value);
		self.t1_latch_hi = hi;
		self.t1_latch_lo = lo;
	}

	fn port_b_bit_7_out(&self) -> bool {
		if self.acr_t1_pb7 {
			self.t1_pb7
		}
		else {
			self.orb.get_flag(7)
		}
	}

	fn mux_enabled(&self) -> bool {
		self.orb.get_flag(ORB_FLAG_MUX_ENABLED)
	}

	fn mux_channel(&self) -> MuxChannel {
		match (self.orb >> 1) & 3  {
			0 => MuxChannel::YAxis,
			1 => MuxChannel::XYAxis,
			2 => MuxChannel::ZAxis,
			3 => MuxChannel::Sound,
			_ => unreachable!()
		}
	}

	fn mux_update(&mut self) {
		let value = self.ora as i8;

		if self.mux_enabled() {
			match self.mux_channel() {
				MuxChannel::YAxis => {
					self.y_sh = value;
				}
				MuxChannel::XYAxis => {
					self.x_sh = value;
					self.y_sh = value;
				}
				MuxChannel::ZAxis => {
					self.z_sh = value;
				}
				_ => {}
			}
		}
	}

	pub fn set_ora(&mut self, ora: u8) {
		self.ora = ora;
		self.mux_update();
	}

	pub fn set_orb(&mut self, orb: u8) {
		self.orb = orb;
		self.mux_update();
	}

	pub fn port_a_out(&self) -> u8 {
		self.ora
	}

	pub fn port_b_out(&self) -> u8 {
		self.orb.with_flag(7, self.port_b_bit_7_out())
	}

	pub fn set_ifr(&mut self, value: u8) {
		if unpack_flag(value, 6) { self.ifr_t1 = false; }
	}
	
	pub fn set_ier(&mut self, value: u8) {
		let bit = value.get_flag(7);
		if unpack_flag(value, 6) { self.ifr_t1 = bit; }
	}

	pub fn ifr(&self) -> u8 {
		let mut ifr = pack_flags([
			false,
			false,
			false,
			false,
			false,
			false,
			self.ifr_t1,
			false
		]);
		
		// Bit 7 is set whenever any interrupt flag is both active and enabled
		if (ifr & self.ier()) != 0 {
			ifr |= 0b1000_0000;
		}
		
		ifr
	}

	pub fn ier(&self) -> u8 {
		pack_flags([
			false,
			false,
			false,
			false,
			false,
			false,
			self.ier_t1,
			true
		])
	}
}

struct ControlLineConfig {
	c1_active_edge: bool,
	c2_config: ControlLine2Config
}

impl Default for ControlLineConfig {
	fn default() -> Self {
		ControlLineConfig::parse(0)
	}
}

impl ControlLineConfig {
	pub fn parse(pcr: u8) -> ControlLineConfig {
		ControlLineConfig {
			c1_active_edge: unpack_flag(pcr, 0),
			c2_config: ControlLine2Config::parse(pcr)
		}
	}
	
	pub fn parse_cb_ca(pcr: u8) -> (ControlLineConfig, ControlLineConfig) {
		let (cb, ca) = unpack_nibbles(pcr);
		(Self::parse(cb), Self::parse(ca))
	}
	
	pub fn encode(&self) -> u8 {
		use self::ControlLine2Config::*;
		let config_bits = match self.c2_config {
			Input { active_edge, independent_interrupt } => {
				((active_edge as u8) << 1) | (independent_interrupt as u8)
			},
			Output(value) => 0b110 | (value as u8),
			HandshakeOutput(value) => 0b100,
			PulseOutput => 0b101
		};
		
		let config_bits = config_bits << 1;
		let c1_active_edge_bit = self.c1_active_edge as u8;
		
		config_bits | c1_active_edge_bit
	}
	
	pub fn encode_cb_ca(cb: &ControlLineConfig, ca: &ControlLineConfig) -> u8 {
		pack_nibbles(cb.encode(), ca.encode())
	}
}

#[derive(Debug)]
enum ControlLine2Config {
	Input {
		active_edge: bool,
		independent_interrupt: bool
	},
	Output(bool),
	HandshakeOutput(bool),
	PulseOutput,
}

impl ControlLine2Config {
	pub fn parse(pcr: u8) -> ControlLine2Config {
		use self::ControlLine2Config::*;
		let pcr = pcr & 0xf;
		
		if unpack_flag(pcr, 3) {
			if unpack_flag(pcr, 2) {
				Output(unpack_flag(pcr, 1))
			}
			else {
				match unpack_flag(pcr, 1) {
					true => PulseOutput,
					false => HandshakeOutput(false)
				}
			}
		}
		else {
			Input {
				active_edge: unpack_flag(pcr, 2),
				independent_interrupt: unpack_flag(pcr, 1)
			}
		}
	}
}