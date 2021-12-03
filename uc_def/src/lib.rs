use std::hash::Hash;

use bitflags::bitflags;

bitflags! {
    pub struct ClassFlags: u32 {
        const INTERFACE = 1 << 0;
        const NATIVE = 1 << 1;
        const CONFIG = 1 << 2;
        const PEROBJECTCONFIG = 1 << 3;
        const IMPLEMENTS = 1 << 4;
        const ABSTRACT = 1 << 5;
        const DEPENDSON = 1 << 6;
    }

    pub struct FuncFlags: u32 {
        const EVENT = 1 << 0;
        const SIMULATED = 1 << 1;
        const NATIVE = 1 << 2;
        const OPERATOR = 1 << 3;
        const PREOPERATOR = 1 << 4;
        const POSTOPERATOR = 1 << 5;
        const STATIC = 1 << 6;
        const FINAL = 1 << 7;
        const EXEC = 1 << 8;
        const LATENT = 1 << 9;
        const PUBLIC = 1 << 10;
        const PRIVATE = 1 << 11;
        const PROTECTED = 1 << 12;
        const COERCE = 1 << 13;
        const ITERATOR = 1 << 14;
        const DELEGATE = 1 << 15;
    }

    pub struct VarFlags: u32 {
        const NATIVE = 1 << 0;
        const CONFIG = 1 << 1;
        const GLOBALCONFIG = 1 << 2;
        const LOCALIZED = 1 << 3;
        const CONST = 1 << 4;
        const PUBLIC = 1 << 5;
        const PROTECTED = 1 << 6;
        const PROTECTEDWRITE = 1 << 7;
        const PRIVATE = 1 << 8;
        const PRIVATEWRITE = 1 << 9;
    }

    pub struct ArgFlags: u32 {
        const COERCE = 1 << 0;
        const CONST = 1 << 1;
        const OPTIONAL = 1 << 2;
        const SKIP = 1 << 3;
        const OUT = 1 << 4;
        const REF = 1 << 5;
        /// For native iterator functions which are known
        /// to not look at their out arguments.
        const OUTONLY = 1 << 6;
    }

    pub struct StructFlags: u32 {

    }
}

pub trait Flags: Copy + Eq + Hash {
    fn into_raw(self) -> u32;
    fn from_raw(bits: u32) -> Self;
}

macro_rules! impl_flags_for_bitflags {
    ($($t:ty),+ $(,)?) => {
        $(
            impl Flags for $t {
                fn into_raw(self) -> u32 {
                    self.bits()
                }

                fn from_raw(bits: u32) -> Self {
                    let ret = <$t>::from_bits_truncate(bits);
                    assert_eq!(bits, ret.bits());
                    ret
                }
            }
        )+
    }
}

impl_flags_for_bitflags! {
    ClassFlags, FuncFlags, VarFlags, ArgFlags, StructFlags,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Op {
    Add,
    AddAdd,
    AddAssign,
    And,
    AndAnd,
    At,
    AtAssign,
    Bang,
    BangEq,
    Div,
    DivAssign,
    Dollar,
    DollarAssign,
    EqEq,
    Gt,
    GtEq,
    GtGt,
    GtGtGt,
    Lt,
    LtEq,
    LtLt,
    Mod,
    Mul,
    MulMul,
    MulAssign,
    Or,
    OrOr,
    Pow,
    PowPow,
    Sub,
    SubAssign,
    SubSub,
    Tilde,
    TildeEq,

    VecCross,
    VecDot,
    YawClockwiseFrom,
}
