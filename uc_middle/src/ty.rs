//! Random notes on subtyping and conversion:
//! * Some types may be converted between each other.
//! * If such a conversion is necessary, the compiler might do it automatically.
//! * Static arrays can't be converted, neither can dynamic arrays.
//! * Some types may be used in place of other types (subtyping)
//! * array<T> is covariant over T, i.e. array<T> is a subtype of array<U>
//!   if and only if T is a subtype of U.
use crate::DefId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BaseTyCtor {
    /// Placeholder type, only valid for native iterator function
    /// arguments where the types aren't known in advance.
    Placeholder,
    /// The type of the `None`-literal, trivially convertible to
    /// object-like types and delegates.
    None,
    /// Base `int` type.
    Int,
    /// Base `float` type.
    Float,
    /// Base `bool` type.
    Bool,
    /// Base `byte` type, or an enum if def index != 0
    /// TODO: Add an Enum type and subtyping/coercion?
    Byte,
    /// Base `string` type.
    String,
    /// Base `name` type.
    Name,
    /// Any struct type, with def index referencing the struct.
    Struct,
    /// Any object type, with def index referencing the class.
    Object,
    /// Any class type, with def index referencing the class.
    Class,
    /// Any interface type, with def index referencing the interface.
    Interface,
    /// Any delegate type, with def index referencing the function.
    Delegate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// The mutually exclusive, non-recursive type decorators.
enum TyDecorator {
    /// The referred-to type, unchanged.
    None,
    /// The result of a native iterator call. The u16 is an index into
    /// a side table linking back to the function definition, the type is
    /// the specific iterable type in the function, if any.
    /// For an iterator over an array<T>, the substitution is T.
    /// For a native function iterator, this is the type of the second argument.
    Iterator(u16),
    /// The `array<T>` type, with the referred-to type in place of the `T`.
    DynArray,
    /// The `T _[X]` type, with the referred-to type in place of the `T`
    /// and the size in place of the `X`.
    StaticArray(u16),
}

#[derive(Copy, Clone, Debug)]
pub struct Ty {
    decorator: TyDecorator,
    base_ctor: BaseTyCtor,
    subst: Option<DefId>,
}

impl Ty {
    /// We don't expose any comparison impls because we want the type checker
    /// to go through is_subtype / classify_conversion. This is a sanity check
    /// that we don't have cycles in our subclassing relationship.
    pub fn assert_literally_same(self, other: Self) {
        assert_eq!(self.decorator, other.decorator);
        assert_eq!(self.base_ctor, other.base_ctor);
        assert_eq!(self.subst, other.subst);
    }
}

#[derive(Debug)]
pub enum ConversionClassification {
    Forbidden,
    Allowed { auto: bool, truncation: bool },
}

impl ConversionClassification {
    const FORBIDDEN: Self = ConversionClassification::Forbidden;
    const ALLOW: Self = ConversionClassification::Allowed {
        auto: false,
        truncation: false,
    };
    const AUTO: Self = ConversionClassification::Allowed {
        auto: true,
        truncation: false,
    };
    const AUTO_T: Self = ConversionClassification::Allowed {
        auto: true,
        truncation: true,
    };
}

/// Whether conversion is possible, whether it has to be explicit, and whether
/// there's any truncation going on when converting. Arrays can never be converted.
pub fn classify_conversion(from: Ty, to: Ty) -> ConversionClassification {
    use BaseTyCtor::*;
    use ConversionClassification as CC;
    if !from.is_undecorated() || !to.is_undecorated() {
        return CC::FORBIDDEN;
    }

    match (from.base_ctor, to.base_ctor) {
        (Byte, Int | Float) => CC::AUTO,
        (Byte, Bool | String) => CC::ALLOW,
        (Int, Byte) => CC::AUTO_T,
        (Int, Float) => CC::AUTO,
        (Int, Bool | String) => CC::ALLOW,
        (Bool, Byte | Int | Float | String) => CC::ALLOW,
        (Float, Bool | String) => CC::ALLOW,
        (Float, Byte | Int) => CC::AUTO_T,
        (Name, Bool | String) => CC::ALLOW,
        (String, Byte | Int | Bool | Float | Name) => CC::ALLOW,

        (Object, Bool | String) => CC::ALLOW,
        (Object, Interface) => CC::AUTO,
        (Object, Class) => CC::ALLOW,
        (Class, Object | String) => CC::ALLOW,
        (Interface, Bool | String) => CC::ALLOW,
        (Interface, Object) => CC::AUTO,

        (Delegate, String) => CC::ALLOW,
        _ => CC::FORBIDDEN,
    }
}

/// Whether specific is a subtype of general
pub fn is_subtype(
    general: Ty,
    specific: Ty,
    object_id: DefId,
    subdef_check: &dyn Fn(DefId, DefId) -> Option<u16>,
) -> Option<u16> {
    use BaseTyCtor::*;

    match (general.decorator, specific.decorator) {
        (TyDecorator::None, TyDecorator::None) => {
            match (general.base_ctor, specific.base_ctor) {
                (Placeholder, _) | (_, Placeholder) => {
                    panic!("attempting to relate placeholder type")
                }
                (None, None) => Some(0),
                (Int, Int) => Some(0),
                (Float, Float) => Some(0),
                (Bool, Bool) => Some(0),
                (Byte, Byte) => {
                    if general.subst == Option::None || general.subst == specific.subst {
                        Some(0)
                    } else {
                        Option::None
                    }
                }
                (String, String) => Some(0),
                (Name, Name) => Some(0),
                // UCC allows struct subtyping but has a soundness hole. Let's
                // start by not allowing it for now.
                (Struct, Struct) => {
                    if specific.subst.unwrap() == general.subst.unwrap() {
                        Some(0)
                    } else {
                        Option::None
                    }
                }
                (Object, None) => Some(0),
                (Object, Object) => subdef_check(general.subst.unwrap(), specific.subst.unwrap()),
                (Object, Class) => {
                    if general.subst.unwrap() == object_id {
                        Some(1)
                    } else {
                        Option::None
                    }
                }
                (Class, Class) => subdef_check(general.subst.unwrap(), specific.subst.unwrap()),
                (Interface, Interface) => {
                    subdef_check(general.subst.unwrap(), specific.subst.unwrap())
                }
                // TODO: Are delegates duck typed?
                (Delegate, None) => Some(0),
                (Delegate, Delegate) => {
                    if general.subst.unwrap() == specific.subst.unwrap() {
                        Some(0)
                    } else {
                        Option::None
                    }
                }
                _ => Option::None,
            }
        }
        (TyDecorator::Iterator(k), TyDecorator::Iterator(l)) => {
            if k == l {
                Some(0)
            } else {
                Option::None
            }
        }
        (TyDecorator::DynArray, TyDecorator::DynArray) => is_subtype(
            Ty {
                decorator: TyDecorator::None,
                ..general
            },
            Ty {
                decorator: TyDecorator::None,
                ..specific
            },
            object_id,
            subdef_check,
        ),
        (TyDecorator::StaticArray(i), TyDecorator::StaticArray(j)) => {
            if i == j {
                is_subtype(
                    Ty {
                        decorator: TyDecorator::None,
                        ..general
                    },
                    Ty {
                        decorator: TyDecorator::None,
                        ..specific
                    },
                    object_id,
                    subdef_check,
                )
            } else {
                Option::None
            }
        }
        _ => Option::None,
    }
}

const _: () = assert!(std::mem::size_of::<Ty>() == std::mem::size_of::<Option<Ty>>());

impl Ty {
    pub const PLACEHOLDER: Ty = Ty::simple(BaseTyCtor::Placeholder);
    pub const NONE: Ty = Ty::simple(BaseTyCtor::None);
    pub const INT: Ty = Ty::simple(BaseTyCtor::Int);
    pub const FLOAT: Ty = Ty::simple(BaseTyCtor::Float);
    pub const BOOL: Ty = Ty::simple(BaseTyCtor::Bool);
    pub const BYTE: Ty = Ty::simple(BaseTyCtor::Byte);
    pub const NAME: Ty = Ty::simple(BaseTyCtor::Name);
    pub const STRING: Ty = Ty::simple(BaseTyCtor::String);

    #[inline]
    const fn simple(ctor: BaseTyCtor) -> Ty {
        Self {
            decorator: TyDecorator::None,
            base_ctor: ctor,
            subst: None,
        }
    }

    #[inline]
    const fn with_def(ctor: BaseTyCtor, id: DefId) -> Ty {
        Self {
            decorator: TyDecorator::None,
            base_ctor: ctor,
            subst: Some(id),
        }
    }

    #[inline]
    pub fn get_def(&self) -> Option<DefId> {
        self.subst
    }

    #[inline]
    pub fn drop_array(&self) -> Self {
        assert!(matches!(
            self.decorator,
            TyDecorator::DynArray | TyDecorator::StaticArray(_)
        ));
        Self {
            decorator: TyDecorator::None,
            ..*self
        }
    }

    #[inline]
    pub fn instanciate_class(&self) -> Self {
        assert!(self.is_undecorated());
        assert!(self.is_class());
        Self {
            base_ctor: BaseTyCtor::Object,
            ..*self
        }
    }

    #[inline]
    pub fn dyn_array_from(inner: Self) -> Ty {
        assert!(inner.is_undecorated());
        Self {
            subst: inner.subst,
            decorator: TyDecorator::DynArray,
            base_ctor: inner.base_ctor,
        }
    }

    #[inline]
    pub fn stat_array_from(inner: Self, count: u16) -> Ty {
        assert!(inner.is_undecorated());
        Self {
            subst: inner.subst,
            decorator: TyDecorator::StaticArray(count),
            base_ctor: inner.base_ctor,
        }
    }

    #[inline]
    pub fn iterator(subst: Self, iterator_idx: u16) -> Ty {
        assert!(subst.is_undecorated());
        Self {
            subst: subst.subst,
            decorator: TyDecorator::Iterator(iterator_idx),
            base_ctor: subst.base_ctor,
        }
    }

    #[inline]
    pub fn object_from(id: DefId) -> Ty {
        Ty::with_def(BaseTyCtor::Object, id)
    }

    #[inline]
    pub fn class_from(id: DefId) -> Ty {
        Ty::with_def(BaseTyCtor::Class, id)
    }

    #[inline]
    pub fn interface_from(id: DefId) -> Ty {
        Ty::with_def(BaseTyCtor::Interface, id)
    }

    #[inline]
    pub fn enum_from(id: DefId) -> Ty {
        Ty::with_def(BaseTyCtor::Byte, id)
    }

    #[inline]
    pub fn struct_from(id: DefId) -> Ty {
        Ty::with_def(BaseTyCtor::Struct, id)
    }

    #[inline]
    pub fn delegate_from(id: DefId) -> Ty {
        Ty::with_def(BaseTyCtor::Delegate, id)
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Class)
    }

    #[inline]
    pub fn is_object(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Object)
    }

    #[inline]
    pub fn is_struct(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Struct)
    }

    #[inline]
    pub fn is_delegate(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Delegate)
    }

    #[inline]
    pub fn is_interface(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Interface)
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Int)
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Float)
    }

    #[inline]
    pub fn is_byte(&self) -> bool {
        self.is_undecorated() && matches!(self.base_ctor, BaseTyCtor::Byte)
    }

    #[inline]
    pub fn is_undecorated(&self) -> bool {
        matches!(self.decorator, TyDecorator::None)
    }

    #[inline]
    pub fn is_dyn_array(&self) -> bool {
        matches!(self.decorator, TyDecorator::DynArray)
    }

    #[inline]
    pub fn is_stat_array(&self) -> bool {
        matches!(self.decorator, TyDecorator::StaticArray(_))
    }

    #[inline]
    pub fn is_iterator(&self) -> bool {
        matches!(self.decorator, TyDecorator::Iterator(_))
    }
}
