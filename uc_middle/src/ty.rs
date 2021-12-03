use crate::DefId;

#[derive(Clone, Copy, Debug)]
enum BaseTyCtor {
    /// Placeholder type, only valid for native iterator function
    /// arguments where the types aren't known in advance.
    Placeholder,
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

#[derive(Clone, Copy, Debug)]
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

const _: () = assert!(std::mem::size_of::<Ty>() == std::mem::size_of::<Option<Ty>>());

impl Ty {
    pub const PLACEHOLDER: Ty = Ty::simple(BaseTyCtor::Placeholder);
    pub const INT: Ty = Ty::simple(BaseTyCtor::Int);
    pub const FLOAT: Ty = Ty::simple(BaseTyCtor::Float);
    pub const BOOL: Ty = Ty::simple(BaseTyCtor::Bool);
    pub const BYTE: Ty = Ty::simple(BaseTyCtor::Byte);
    pub const NAME: Ty = Ty::simple(BaseTyCtor::Name);
    pub const STRING: Ty = Ty::simple(BaseTyCtor::String);

    const fn simple(ctor: BaseTyCtor) -> Ty {
        Self {
            decorator: TyDecorator::None,
            base_ctor: ctor,
            subst: None,
        }
    }

    const fn with_def(ctor: BaseTyCtor, id: DefId) -> Ty {
        Self {
            decorator: TyDecorator::None,
            base_ctor: ctor,
            subst: Some(id),
        }
    }

    pub fn get_def(&self) -> Option<DefId> {
        self.subst
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
