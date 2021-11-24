use std::num::NonZeroU32;

use crate::DefId;

const ARRAY_MASK: u32 = 1 << 31;
const BASE_TY_MASK: u32 = 0xF << 27;
const DEF_ID_MASK: u32 = 0x7FFFFFF;

pub const MAX_DEF_ID: u32 = DEF_ID_MASK;

const __ASSERT_COVERED: () = {
    let assert_mask_covers_all_bits = [()];
    let combined_mask = ARRAY_MASK | BASE_TY_MASK | DEF_ID_MASK;
    assert_mask_covers_all_bits[((combined_mask != !0) as bool) as usize]
};

#[derive(Clone, Copy, Debug)]
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

impl std::fmt::Debug for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_tuple("Ty");
        builder.field(&self.base_ty());
        if let Some(id) = self.get_def() {
            builder.field(&id);
        }
        builder.finish()
    }
}

impl Ty {
    pub const INT: Self = Ty(NonZeroU32::new(BaseTy::Int as u32).unwrap());
    pub const FLOAT: Self = Ty(NonZeroU32::new(BaseTy::Float as u32).unwrap());
    pub const BOOL: Self = Ty(NonZeroU32::new(BaseTy::Bool as u32).unwrap());
    pub const BYTE: Self = Ty(NonZeroU32::new(BaseTy::Byte as u32).unwrap());
    pub const STRING: Self = Ty(NonZeroU32::new(BaseTy::String as u32).unwrap());
    pub const NAME: Self = Ty(NonZeroU32::new(BaseTy::Name as u32).unwrap());

    pub fn get_def(&self) -> Option<NonZeroU32> {
        if self.is_array() {
            return None;
        }
        match self.base_ty() {
            BaseTy::Int | BaseTy::Float | BaseTy::Bool | BaseTy::String | BaseTy::Name => None,
            BaseTy::Struct
            | BaseTy::Object
            | BaseTy::Class
            | BaseTy::Interface
            | BaseTy::Delegate => Some(NonZeroU32::new(self.0.get() & DEF_ID_MASK).unwrap()),
            BaseTy::Byte => NonZeroU32::new(self.0.get() & DEF_ID_MASK),
        }
    }

    fn base_ty(&self) -> BaseTy {
        match self.0.get() & BASE_TY_MASK {
            const { BaseTy::Int as u32 } => BaseTy::Int,
            const { BaseTy::Float as u32 } => BaseTy::Float,
            const { BaseTy::Bool as u32 } => BaseTy::Bool,
            const { BaseTy::Byte as u32 } => BaseTy::Byte,
            const { BaseTy::String as u32 } => BaseTy::String,
            const { BaseTy::Name as u32 } => BaseTy::Name,
            const { BaseTy::Struct as u32 } => BaseTy::Struct,
            const { BaseTy::Object as u32 } => BaseTy::Object,
            const { BaseTy::Class as u32 } => BaseTy::Class,
            const { BaseTy::Interface as u32 } => BaseTy::Interface,
            const { BaseTy::Delegate as u32 } => BaseTy::Delegate,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn array_from(inner: Self) -> Ty {
        assert!(inner.is_array());
        Ty(NonZeroU32::new(inner.0.get() | ARRAY_MASK).unwrap())
    }

    #[inline]
    pub fn object_from(id: DefId) -> Ty {
        Ty(NonZeroU32::new(BaseTy::Object as u32 | id.0.get()).unwrap())
    }

    #[inline]
    pub fn interface_from(id: DefId) -> Ty {
        Ty(NonZeroU32::new(BaseTy::Interface as u32 | id.0.get()).unwrap())
    }

    #[inline]
    pub fn enum_from(id: DefId) -> Ty {
        Ty(NonZeroU32::new(BaseTy::Byte as u32 | id.0.get()).unwrap())
    }

    #[inline]
    pub fn struct_from(id: DefId) -> Ty {
        Ty(NonZeroU32::new(BaseTy::Struct as u32 | id.0.get()).unwrap())
    }

    #[inline]
    pub fn delegate_from(id: DefId) -> Ty {
        Ty(NonZeroU32::new(BaseTy::Delegate as u32 | id.0.get()).unwrap())
    }

    #[inline]
    pub fn is_array(&self) -> bool {
        self.0.get() & ARRAY_MASK != 0
    }
}
