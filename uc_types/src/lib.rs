use std::num::NonZeroU32;

const ARRAY_MASK: u32 = 1 << 31;
const BASE_TY_MASK: u32 = 0xF << 27;
const DEF_ID_MASK: u32 = 0x7FFFFFF;

const __ASSERT_COVERED: () = {
    let assert_mask_covers_all_bits = [()];
    let combined_mask = ARRAY_MASK | BASE_TY_MASK | DEF_ID_MASK;
    assert_mask_covers_all_bits[((combined_mask != !0) as bool) as usize]
};

enum BaseTy {
    /// Base `int` type.
    Int = 1 << 27,
    /// Base `float` type.
    Float = 2 << 27,
    /// Base `bool` type.
    Bool = 3 << 27,
    /// Base `byte` type, or an enum if def index != 0
    Byte = 4 << 27,
    /// Base `string` type.
    String = 5 << 27,
    /// Base `name` type.
    Name = 6 << 27,
    /// Any struct type, with def index referencing the struct.
    Struct = 7 << 27,
    /// Any object type, with def index referencing the class.
    Object = 8 << 27,
    /// Any class type, with def index referencing the class.
    Class = 9 << 27,
    /// Any interface type, with def index referencing the interface.
    Interface = 10 << 27,
    /// Any delegate type, with def index referencing the function.
    Delegate = 11 << 27,
}

/// Compact representation of an UnrealScript type.
/// - NonZero to accomodate Option/None optimization.
/// - array<_> represented with a single bit flag because
///   array<array<_>> is not supported by the language.
/// - base type represented with 4 bits
/// - 27 bits for indexing back into the thing that defined the
///   type (134 million classes+structs+enums+interfaces).
#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(transparent)]
pub struct Ty(NonZeroU32);

impl Ty {
    pub const INT: Self = Ty(unsafe { NonZeroU32::new_unchecked(BaseTy::Int as u32) });
    pub const FLOAT: Self = Ty(unsafe { NonZeroU32::new_unchecked(BaseTy::Float as u32) });
    pub const BOOL: Self = Ty(unsafe { NonZeroU32::new_unchecked(BaseTy::Bool as u32) });
    pub const BYTE: Self = Ty(unsafe { NonZeroU32::new_unchecked(BaseTy::Byte as u32) });
    pub const STRING: Self = Ty(unsafe { NonZeroU32::new_unchecked(BaseTy::String as u32) });
    pub const NAME: Self = Ty(unsafe { NonZeroU32::new_unchecked(BaseTy::Name as u32) });
}
