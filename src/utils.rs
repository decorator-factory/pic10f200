#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bit {
    I,
    O,
}

impl Bit {
    pub fn is_set(self) -> bool {
        match self {
            Bit::I => true,
            Bit::O => false,
        }
    }

    pub fn as_u8(self) -> u8 {
        match self {
            Bit::I => 1,
            Bit::O => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct U4(u8);

impl U4 {
    pub fn from_u8(val: u8) -> Option<Self> {
        if val < (1 << 4) {
            Some(U4(val))
        } else {
            None
        }
    }

    pub fn get<T>(self, arr: &[T; 16]) -> &T {
        // SAFETY: it's impossible to create a U4 that wouldn't be in 0..=15

        unsafe { arr.get_unchecked(self.0 as usize) }
    }

    pub fn put<T>(self, arr: &mut [T; 16], value: T) {
        // SAFETY: it's impossible to create a U4 that wouldn't be in 0..=15

        let ptr = unsafe { arr.get_unchecked_mut(self.0 as usize) };
        *ptr = value
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct U3(u8);

impl U3 {
    pub fn from_u8(val: u8) -> Option<Self> {
        if val < (1 << 3) {
            Some(U3(val))
        } else {
            None
        }
    }

    pub fn from_bits(b2: Bit, b1: Bit, b0: Bit) -> Self {
        U3(b2.as_u8() * 4 + b1.as_u8() * 2 + b0.as_u8())
    }

    pub fn as_u8(self) -> u8 {
        self.0
    }

    pub fn is_in(&self, value: u8) -> bool {
        value & (1u8 << self.0) == 0
    }
}
