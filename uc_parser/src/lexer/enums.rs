use uc_def::Op;
use uc_name::Ascii;

use once_cell::sync::Lazy;

use std::{collections::HashMap, str::FromStr};

macro_rules! sigils {
    ($(#[$metas:meta])* $v:vis enum $name:ident { $($variant:ident => $op:ident),* $(,)? } ) => {
        $(#[$metas])* $v enum $name {
            $(
                $variant,
            )*
        }

        impl $name {
            pub fn to_op(&self) -> Option<Op> {
                match self {
                    $($name::$variant => sigils!(@ $op, $variant ),)*
                }
            }
        }
    };

    (@ Same, $var:ident) => { Some(Op::$var) };
    (@ None, $var:ident) => { None };
}

sigils! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    pub enum Sigil {
        Add => Same,
        AddAdd => Same,
        AddAssign => Same,
        And => Same,
        AndAnd => Same,
        At => Same,
        AtAssign => Same,
        Bang => Same,
        BangEq => Same,
        Div => Same,
        DivAssign => Same,
        Dollar => Same,
        DollarAssign => Same,
        EqEq => Same,
        Gt => Same,
        GtEq => Same,
        GtGt => Same,
        GtGtGt => Same,
        Lt => Same,
        LtEq => Same,
        LtLt => Same,
        Mod => Same,
        Mul => Same,
        MulMul => Same,
        MulAssign => Same,
        Or => Same,
        OrOr => Same,
        Pow => Same,
        PowPow => Same,
        Sub => Same,
        SubAssign => Same,
        SubSub => Same,
        Tilde => Same,
        TildeEq => Same,

        Colon => None,
        Dot => None,
        Eq => None,
        Tern => None,
        LBrack => None,
        RBrack => None,
        LParen => None,
        RParen => None,
        LBrace => None,
        RBrace => None,
    }
}

#[derive(Debug, Clone, Copy)]
pub struct VariantNotFound;

macro_rules! keywords {
    ($(#[$metas:meta])* $v:vis enum $name:ident { $($variant:ident),* $(,)? } ) => {
        $(#[$metas])* $v enum $name {
            $(
                $variant,
            )*
        }

        impl AsRef<str> for $name {
            fn as_ref(&self) -> &str {
                match self {
                    $($name::$variant => stringify!($variant),)*
                }
            }
        }

        impl FromStr for $name {
            type Err = VariantNotFound;

            #[track_caller]
            #[inline]
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                let key = Ascii::new(s).expect("non-ascii key");
                CASE_INSENSITIVE_LOOKUP.get(&key).copied().ok_or(VariantNotFound)
            }
        }

        static CASE_INSENSITIVE_LOOKUP: Lazy<HashMap<Ascii<&'static str>, $name>> = Lazy::new(|| {
            let mut map = HashMap::new();

            $(
                map.insert(Ascii::new(stringify!($variant)).unwrap(), $name::$variant);
            )*

            map
        });
    };
}

keywords! {
    #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
    pub enum Keyword {
        Abstract,
        Array,
        Auto,
        AutoExpandCategories,
        Break,
        Case,
        Class,
        ClassGroup,
        Client,
        Coerce,
        CollapseCategories,
        Config,
        Const,
        Continue,
        CppText,
        Cross,
        CrossLevelPassive,
        DataBinding,
        Default,
        DefaultProperties,
        Delegate,
        DependsOn,
        Deprecated,
        Do,
        DontCollapseCategories,
        DontSortCategories,
        Dot,
        DuplicateTransient,
        EditConst,
        EditHide,
        EditFixedSize,
        EditInline,
        EditInlineNew,
        EditInlineUse,
        EditorOnly,
        EditTextBox,
        Else,
        Enum,
        Event,
        Exec,
        Export,
        Extends,
        Final,
        For,
        ForceScriptOrder,
        Foreach,
        Function,
        GlobalConfig,
        Goto,
        HideCategories,
        If,
        Ignores,
        Immutable,
        ImmutableWhenCooked,
        Implements,
        Inherits,
        Init,
        Input,
        Instanced,
        Interface,
        Interp,
        Iterator,
        Latent,
        Local,
        Localized,
        Map,
        Native,
        NativeReplication,
        New,
        NoClear,
        NoExport,
        NoExportHeader,
        NoImport,
        None,
        NonTransactional,
        NotForConsole,
        NotPlaceable,
        Operator,
        Optional,
        Out,
        PerObjectConfig,
        Placeable,
        PostOperator,
        PreOperator,
        Private,
        PrivateWrite,
        Protected,
        ProtectedWrite,
        Public,
        Ref,
        Reliable,
        Replication,
        RepNotify,
        RepRetry,
        Return,
        SerializeText,
        Server,
        ShowCategories,
        Simulated,
        Singular,
        Skip,
        State,
        StateOnly,
        Static,
        Struct,
        StructCppText,
        StructDefaultProperties,
        Switch,
        Transient,
        Unreliable,
        Until,
        Var,
        Virtual,
        While,
        Within,
    }
}
