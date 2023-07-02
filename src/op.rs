use crate::utils::{
    Bit::{self, *},
    U3, U4,
};

#[derive(Debug, Clone, Copy)]
pub struct ProgramWord(u16);

impl ProgramWord {
    pub fn from_u16(val: u16) -> Option<Self> {
        if val < (1 << 12) {
            Some(ProgramWord(val))
        } else {
            None
        }
    }

    pub fn bits(self) -> [Bit; 12] {
        fn get_bit_u16(shift: u8, value: u16) -> Bit {
            if (value & (1 << shift)) != 0 {
                I
            } else {
                O
            }
        }

        let ProgramWord(w) = self;
        [
            get_bit_u16(11, w),
            get_bit_u16(10, w),
            get_bit_u16(9, w),
            get_bit_u16(8, w),
            get_bit_u16(7, w),
            get_bit_u16(6, w),
            get_bit_u16(5, w),
            get_bit_u16(4, w),
            get_bit_u16(3, w),
            get_bit_u16(2, w),
            get_bit_u16(1, w),
            get_bit_u16(0, w),
        ]
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    Indirect,            // INDF
    Timer,               // TMR0
    ProgramCounterLower, // PCL
    Status,              // STATUS
    Pointer,             // FSR
    Oscillator,          // OSCCAL
    Gpio,                // GPIO
    GeneralPurpose(U4),
}

impl Reg {
    pub fn from_u8(raw: u8) -> Option<Reg> {
        match raw {
            0x00 => Some(Reg::Indirect),
            0x01 => Some(Reg::Timer),
            0x02 => Some(Reg::ProgramCounterLower),
            0x03 => Some(Reg::Status),
            0x04 => Some(Reg::Pointer),
            0x05 => Some(Reg::Oscillator),
            0x06 => Some(Reg::Gpio),
            x if (0x10..=0x1f).contains(&x) => U4::from_u8(x).map(Reg::GeneralPurpose),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// Destination mode. Describes where the result of an instruction is stored.
///
pub enum DestMode {
    /// Result is stored back into the original register
    Inplace,

    /// Result is stored in the W register
    IntoW,
}

impl DestMode {
    pub fn from_bit(bit: Bit) -> DestMode {
        match bit {
            I => DestMode::Inplace,
            O => DestMode::IntoW,
        }
    }
}

/// Single instruction, as inderstood by the processor
///
/// Refer to the PIC10F200 datasheet for the exact meaning of each instruction
///
#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add(Reg, DestMode),          // ADDWF
    And(Reg, DestMode),          // ANDWF
    AndConst(u8),                // ANDLW
    ClearBit(U3, Reg),           // BCF
    SetBit(U3, Reg),             // BSF
    SkipIfBitClear(U3, Reg),     // BTFSC
    SkipIfBitSet(U3, Reg),       // BTFSS
    Call(u8),                    // CALL
    Clear(Reg),                  // CLRF
    ClearW,                      // CLRW
    ClearWatchdogTimer,          // CLRWDT
    Complement(Reg, DestMode),   // COMF
    Decrement(Reg, DestMode),    // DECF
    DecrSkipZero(Reg, DestMode), // DECFSZ
    Goto(u8),                    // GOTO
    Increment(Reg, DestMode),    // INCF
    IncrSkipZero(Reg, DestMode), // INCFSZ
    OrConst(u8),                 // IORLW
    Or(Reg, DestMode),           // IORWF
    Move(Reg, DestMode),         // MOVF
    MoveConstToW(u8),            // MOVLW
    MoveFromW(Reg),              // MOVWF
    Nop,                         // NOP
    Option,                      // OPTION
    ReturnLiteral(u8),           // RETLW
    RotateLeft(Reg, DestMode),   // RLF
    RotateRight(Reg, DestMode),  // RRF
    Sleep,                       // SLEEP
    Sub(Reg, DestMode),          // SUBWF
    SwapNibbles(Reg, DestMode),  // SWAPF
    TriStateFromW,               // TRIS
    XorConst(u8),                // XORLW
    Xor(Reg, DestMode),          // XORWF
}

#[rustfmt::skip]
pub fn parse_op(word: ProgramWord) -> Option<Op> {
    use Op::*;

    fn build_u8(slice: &[Bit]) -> u8 {
        slice
            .iter()
            .rev()
            .fold(0, |acc, &b| if b.is_set() { acc * 2 + 1 } else { acc })
    }

    match word.bits() {
        [O, O, O, I, I, I, d, f @ ..] => Some(
            Add(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),
        [O, O, O, I, O, I, d, f @ ..] => Some(
            And(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),
        [O, O, O, O, O, I, I, f @ ..] => Some(
            Clear(Reg::from_u8(build_u8(&f))?)
        ),
        [O, O, O, O, O, I, O, O, O, O, O, O] => Some(
            ClearW
        ),

        [O, O, I, O, O, I, d, f @ ..] => Some(
            Complement(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),

        [O, O, O, O, I, I, d, f @ ..] => Some(
            Decrement(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),
        [O, O, I, O, I, I, d, f @ ..] => Some(
            DecrSkipZero(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),

        [O, O, I, O, I, O, d, f @ ..] => Some(
            Increment(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),
        [O, O, I, I, I, I, d, f @ ..] => Some(
            IncrSkipZero(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),

        [O, O, O, I, O, O, d, f @ ..] => Some(
            Or(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),
        [O, O, I, O, O, O, d, f @ ..] => Some(
            Move(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),
        [O, O, O, O, O, O, I, f @ ..] => Some(
            MoveFromW(Reg::from_u8(build_u8(&f))?)
        ),
        [O, O, O, O, O, O, O, O, O, O, O, O] => Some(
            Nop
        ),

        [O, O, I, I, O, I, d, f @ ..] => Some(
            RotateLeft(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),
        [O, O, I, I, O, O, d, f @ ..] => Some(
            Sub(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),

        [O, O, I, I, I, O, d, f @ ..] => Some(
            SwapNibbles(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),

        [O, O, O, I, I, O, d, f @ ..] => Some(
            Xor(Reg::from_u8(build_u8(&f))?, DestMode::from_bit(d))
        ),

        // Bit-oriented
        [O, I, O, O, b2, b1, b0, f @ ..] => Some(
            ClearBit(U3::from_bits(b2, b1, b0), Reg::from_u8(build_u8(&f))?)
        ),
        [O, I, O, I, b2, b1, b0, f @ ..] => Some(
            SetBit(U3::from_bits(b2, b1, b0), Reg::from_u8(build_u8(&f))?)
        ),
        [O, I, I, O, b2, b1, b0, f @ ..] => Some(
            SkipIfBitClear(U3::from_bits(b2, b1, b0), Reg::from_u8(build_u8(&f))?)
        ),
        [O, I, I, I, b2, b1, b0, f @ ..] => Some(
            SkipIfBitSet(U3::from_bits(b2, b1, b0), Reg::from_u8(build_u8(&f))?)
        ),

        // Literals
        [I, I, I, O, k @ ..] => Some(
            AndConst(build_u8(&k))
        ),
        [I, I, O, I, k @ ..] => Some(
            OrConst(build_u8(&k))
        ),
        [I, I, O, O, k @ ..] => Some(
            MoveConstToW(build_u8(&k))
        ),
        [I, I, I, I, k @ ..] => Some(
            XorConst(build_u8(&k))
        ),

        // Control flow
        [I, O, O, I, k @ ..] => Some(
            Call(build_u8(&k))
        ),
        [I, O, I, O, k @ ..] => Some(
            Goto(build_u8(&k))
        ),

        _ => None,
    }
}
