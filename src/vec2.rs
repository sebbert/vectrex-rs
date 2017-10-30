#![allow(unused)]

use std::ops::*;
use std::fmt;

#[derive(Default,Clone,Copy)]
pub struct Vec2 {
	pub x: i32,
	pub y: i32
}

impl Vec2 {
	pub fn new(x: i32, y: i32) -> Vec2 {
		Vec2 { x:x, y:y }
	}
}

impl Add for Vec2 {
	type Output = Vec2;
	
	fn add(self, rhs: Vec2) -> Vec2 {
		Vec2 {
			x: self.x + rhs.x,
			y: self.y + rhs.y
		}
	}
}

impl Neg for Vec2 {
	type Output = Vec2;

	fn neg(self) -> Vec2 {
		Vec2 {
			x: -self.x,
			y: -self.y
		}
	}
}

impl Sub for Vec2 {
	type Output = Vec2;

	fn sub(self, rhs: Vec2) -> Vec2 {
		self.add(-rhs)
	}
}

impl Mul<i32> for Vec2 {
	type Output = Vec2;

	fn mul(self, rhs: i32) -> Vec2 {
		Vec2 {
			x: self.x * rhs,
			y: self.y * rhs
		}
	}
}

impl Div<i32> for Vec2 {
	type Output = Vec2;

	fn div(self, rhs: i32) -> Vec2 {
		Vec2 {
			x: self.x / rhs,
			y: self.y / rhs
		}
	}
}

impl AddAssign for Vec2 {
	fn add_assign(&mut self, rhs: Vec2) {
		*self = *self + rhs;
	} 
}

impl fmt::Debug for Vec2 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "Vec2({}, {})", self.x, self.y)
	}
}