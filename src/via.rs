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

const IR_FLAG_CA2: usize = 0;
const IR_FLAG_CA1: usize = 1;
const IR_FLAG_SHIFT_REG: usize = 2;
const IR_FLAG_CB2: usize = 3;
const IR_FLAG_CB1: usize = 4;
const IR_FLAG_TIMER_2: usize = 5;
const IR_FLAG_TIMER_1: usize = 6;
const IFR_FLAG_ENABLED: usize = 7;
const IER_FLAG_SET_CLEAR: usize = 7;

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

#[derive(Default,Debug)]
pub struct Via {
	ier: u8,
	ifr: u8,

	t1_counter_lo: u8,
	t1_counter_hi: u8,
	t1_latch_lo: u8,
	t1_latch_hi: u8,
	t1_pb7: bool,
	t1_running: bool,

	acr: AuxControlRegister,

	sr: u8,
	sr_bit_counter: u8,
	sr_counter: u8,
	sr_clock: bool,

	ora: u8,
	ddra: u8,

	orb: u8,
	ddrb: u8,
	
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
		let mut via: Via = Default::default();
		via.sr_bit_counter = 8;
		via
	}

	pub fn step(&mut self, line_sink: &mut LineSink) -> bool {
		if self.t1_running {
			let next_t1_counter = self.t1_counter().wrapping_sub(1);
			self.set_t1_counter(next_t1_counter);
			
			if next_t1_counter == 0xffff {
				self.set_ifr(IR_FLAG_TIMER_1);
				self.t1_pb7 = !self.t1_pb7;
				let latch = self.t1_latch();
				self.set_t1_counter(latch);

				if !self.acr.t1_control.continuous {
					self.t1_running = !self.t1_running;
				}
			}
		}

		if self.sr_bit_counter < 8 {
			match &self.acr.shift_mode {
				&ShiftRegisterMode::ShiftOut(ref clock) => {
					match clock {
						&ShiftRegisterClock::Clock2 => {
							let hbit = self.sr.get_flag(7) as u8;
							self.sr = (self.sr << 1) | hbit;
							self.sr_bit_counter += 1;
						}
						_ => {}
					}
				}
				_ => {}
			};

			if self.sr_bit_counter == 8 {
				self.set_ifr(IR_FLAG_SHIFT_REG);
			}
		}

		self.pulse_state = false;
		self.hardware_step(line_sink);
		self.pulse_state = true;
		
		(self.read_ier() & self.read_ifr()) & 0x7f != 0
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
		debug!("VIA read: {:x}", addr);
		match addr {
			REG_T1_COUNTER_LO => {
				self.clear_ifr(IR_FLAG_TIMER_1);
				self.t1_counter_lo
			}
			REG_T1_COUNTER_HI => self.t1_counter_hi,
			REG_T1_LATCH_LO => self.t1_latch_lo,
			REG_T1_LATCH_HI => self.t1_latch_hi,
			REG_IER => self.read_ier(),
			REG_IFR => self.read_ifr(),
			REG_PCR => self.read_pcr(),
			REG_PORT_B => self.read_orb(),
			REG_PORT_A => self.read_ora(),
			REG_SHIFT => self.read_sr(),
			REG_ACR => self.read_acr(),
			_ => {
				error!("Read from unimplemented VIA reg {:01x}", addr);
				0
			}
		}
	}

	pub fn write(&mut self, addr: u16, value: u8) {
		let addr = Self::mask_addr(addr);
		debug!("VIA write: {:x} = {:02x}", addr, value);
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
				self.clear_ifr(IR_FLAG_TIMER_1);
				self.t1_pb7 = false;

				self.t1_running = true;
			},
			REG_IER => self.write_ier(value),
			REG_IFR => self.write_ifr(value),
			REG_PCR => self.write_pcr(value),
			REG_PORT_A => self.write_ora(value),
			REG_PORT_B => self.write_orb(value),
			REG_PORT_A_NO_HANDSHAKE => self.write_ora(value),
			REG_SHIFT => self.write_sr(value),
			REG_ACR => self.write_acr(value),
			_ => warn!("Write to unimplemented VIA reg {:01x} = {:02x}", addr, value)
		}
	}
	
	#[allow(unused_variables)]
	fn control_line_2(&self, pcr: &ControlLineConfig) -> bool {
		use self::ControlLine2Config::*;
		match pcr.c2_config {
			Input { active_edge, independent_interrupt } => {
				// debug!("Control line 2 read while in input mode");
				true
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
	
	fn read_pcr(&self) -> u8 {
		ControlLineConfig::encode_cb_ca(&self.pcr_cb, &self.pcr_ca)
	}
	
	fn write_pcr(&mut self, pcr: u8) {
		let (cb, ca) = ControlLineConfig::parse_cb_ca(pcr);
		
		self.pcr_cb = cb;
		self.pcr_ca = ca;
	}

	fn mask_addr(addr: u16) -> u16 {
		addr & 0xf
	}

	fn trigger_sr(&mut self) {
		self.sr_bit_counter = 0;
		self.clear_ifr(IR_FLAG_SHIFT_REG);
	}

	fn read_sr(&mut self) -> u8 {
		self.trigger_sr();
		self.sr
	}

	fn write_sr(&mut self, value: u8) {
		self.trigger_sr();
		self.sr = value;
	}

	fn t1_counter(&self) -> u16 {
		pack_16(self.t1_counter_hi, self.t1_counter_lo)
	}

	fn t1_latch(&self) -> u16 {
		pack_16(self.t1_latch_hi, self.t1_latch_lo)
	}

	fn set_t1_counter(&mut self, value: u16) {
		let (hi, lo) = unpack_16(value);
		self.t1_counter_hi = hi;
		self.t1_counter_lo = lo;
	}

	fn set_t1_latch(&mut self, value: u16) {
		let (hi, lo) = unpack_16(value);
		self.t1_latch_hi = hi;
		self.t1_latch_lo = lo;
	}

	fn port_b_bit_7_out(&self) -> bool {
		if self.acr.t1_control.pb7 {
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

	pub fn read_orb(&self) -> u8 {
		self.orb & self.ddrb
	}

	pub fn read_ora(&self) -> u8 {
		0
	}

	pub fn write_ora(&mut self, ora: u8) {
		self.ora = ora;
		self.mux_update();
	}

	pub fn write_orb(&mut self, orb: u8) {
		self.orb = orb;
		self.mux_update();
	}

	fn clear_ifr(&mut self, flag: usize) {
		self.ifr = self.ifr.set_flag(flag, false);
	}

	fn set_ifr(&mut self, flag: usize) {
		self.ifr = self.ifr.set_flag(flag, true);
	}

	pub fn write_ifr(&mut self, value: u8) {
		let value = value & 0x7f;
		self.ifr = self.ifr & !value;
	}
	
	pub fn write_ier(&mut self, value: u8) {
		let bit = value.get_flag(IER_FLAG_SET_CLEAR);
		let value = value.set_flag(IER_FLAG_SET_CLEAR, false);
		if bit {
			self.ier |= value;
		}
		else {
			self.ier &= !value;
		}
	}

	pub fn read_ifr(&self) -> u8 {
		self.ifr.set_flag(7, (self.ifr & self.ier & 0x7f) != 0)
	}

	pub fn read_ier(&self) -> u8 {
		self.ier.set_flag(IER_FLAG_SET_CLEAR, true)
	}

	fn read_acr(&self) -> u8 {
		self.acr.encode()
	}

	fn write_acr(&mut self, value: u8) {
		self.acr = AuxControlRegister::parse(value);
	}
}

#[derive(Debug)]
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
			c1_active_edge: pcr.get_flag(0),
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
			HandshakeOutput(_) => 0b100,
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
		
		if pcr.get_flag(3) {
			if pcr.get_flag(2) {
				Output(pcr.get_flag(1))
			}
			else {
				match pcr.get_flag(1) {
					true => PulseOutput,
					false => HandshakeOutput(false)
				}
			}
		}
		else {
			Input {
				active_edge: pcr.get_flag(2),
				independent_interrupt: pcr.get_flag(1)
			}
		}
	}
}

#[derive(Debug)]
enum ShiftRegisterClock {
	Timer2,
	Clock2,
	ExtClock
}

impl ShiftRegisterClock {
	fn parse(bits: u8) -> ShiftRegisterClock {
		match bits & 0b11 {
			0 => panic!("Invalid shift register clock"),
			1 => ShiftRegisterClock::Timer2,
			2 => ShiftRegisterClock::Clock2,
			3 => ShiftRegisterClock::ExtClock,
			_ => unreachable!()
		}
	}

	fn encode(&self) -> u8 {
		use self::ShiftRegisterClock::*;
		match self {
			&Timer2 => 1,
			&Clock2 => 2,
			&ExtClock => 3
		}
	}
}

#[derive(Debug)]
enum ShiftRegisterMode {
	Disabled,
	ShiftOutFreeRunningT2,
	ShiftOut(ShiftRegisterClock),
	ShiftIn(ShiftRegisterClock),
}

impl Default for ShiftRegisterMode {
	fn default() -> Self {
		ShiftRegisterMode::Disabled
	}
}

impl ShiftRegisterMode {
	fn parse(mode_bits: u8) -> ShiftRegisterMode {
		let mode_bits = mode_bits & 0b111;
		match mode_bits {
			0b000 => ShiftRegisterMode::Disabled,
			0b100 => ShiftRegisterMode::ShiftOutFreeRunningT2,
			_ => {
				let clock = ShiftRegisterClock::parse(mode_bits);
				match mode_bits.get_flag(2) {
					true => ShiftRegisterMode::ShiftOut(clock),
					false => ShiftRegisterMode::ShiftIn(clock)
				}
			}
		}
	}

	fn encode(&self) -> u8 {
		use self::ShiftRegisterMode::*;
		match self {
			&Disabled => 0b000,
			&ShiftOutFreeRunningT2 => 0b100,
			&ShiftOut(ref clock) => 0b100 | clock.encode(),
			&ShiftIn(ref clock) => clock.encode()
		}
	}
}

#[derive(Default,Debug)]
struct AuxControlRegister {
	latch_pa: bool,
	latch_pb: bool,
	shift_mode: ShiftRegisterMode,
	t1_control: Timer1Control,
	t2_control: Timer2Control,
}

impl AuxControlRegister {
	fn parse(acr: u8) -> AuxControlRegister {
		AuxControlRegister {
			latch_pa: acr.get_flag(0),
			latch_pb: acr.get_flag(1),
			shift_mode: ShiftRegisterMode::parse(acr >> 2),
			t2_control: Timer2Control::parse(acr >> 5),
			t1_control: Timer1Control::parse(acr >> 6),
		}
	}

	fn encode(&self) -> u8 {
		let mut acr: u8 = 0;
		
		acr = acr.set_flag(0, self.latch_pa);
		acr = acr.set_flag(1, self.latch_pb);
		acr |= self.shift_mode.encode() << 2;
		acr |= self.t2_control.encode() << 5;
		acr |= self.t1_control.encode() << 6;
		
		acr
	}
}

#[derive(Default,Debug)]
struct Timer1Control {
	continuous: bool,
	pb7: bool
}

impl Timer1Control {
	fn parse(bits: u8) -> Timer1Control {
		Timer1Control {
			continuous: bits.get_flag(0),
			pb7: bits.get_flag(1)
		}
	}

	fn encode(&self) -> u8 {
		0.set_flag(0, self.continuous)
		 .set_flag(1, self.pb7)
	}
}

#[derive(Debug)]
enum Timer2Control {
	TimedInterrupt,
	CountDown
}

impl Default for Timer2Control {
	fn default() -> Timer2Control {
		Timer2Control::TimedInterrupt
	}
}

impl Timer2Control {
	fn parse(bits: u8) -> Timer2Control {
		match bits & 1 {
			0 => Timer2Control::TimedInterrupt,
			_ => Timer2Control::CountDown
		}
	}

	fn encode(&self) -> u8 {
		match self {
			&Timer2Control::TimedInterrupt => 0,
			&Timer2Control::CountDown => 1
		}
	}
}