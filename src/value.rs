#[cfg(not(feature = "nan_boxing"))]
mod value_inner {
    use std::fmt::Display;

    use crate::{memory::Ref, object::print_object};

    // TODO: Clone? Copy?
    #[derive(Clone, Copy)]
    pub enum Value {
        Bool(bool),
        Nil,
        Number(f64),
        Ref(Ref<()>),
    }

    impl Value {
        pub const NIL: Value = Value::Nil;

        pub fn is_nil(&self) -> bool {
            match self {
                Value::Nil => true,
                _ => false,
            }
        }

        pub fn _as_bool(&self) -> Option<bool> {
            match self {
                Value::Bool(b) => Some(*b),
                _ => None,
            }
        }

        pub fn as_number(&self) -> Option<f64> {
            match self {
                Value::Number(n) => Some(*n),
                _ => None,
            }
        }

        pub fn as_erased_ref(&self) -> Option<Ref<()>> {
            match self {
                Value::Ref(r) => Some(*r),
                _ => None,
            }
        }

        pub fn is_falsey(&self) -> bool {
            match self {
                Value::Nil | Value::Bool(false) => true,
                _ => false,
            }
        }
    }

    impl From<bool> for Value {
        fn from(b: bool) -> Self {
            Value::Bool(b)
        }
    }

    impl From<f64> for Value {
        fn from(n: f64) -> Self {
            Value::Number(n)
        }
    }

    impl<T> From<Ref<T>> for Value {
        fn from(r: Ref<T>) -> Self {
            Value::Ref(r.with_type_erased())
        }
    }

    impl Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Value::Bool(b) => write!(f, "{:?}", b),
                Value::Nil => write!(f, "nil"),
                Value::Number(n) => write!(f, "{}", n),
                Value::Ref(_r) => print_object(self, f),
            }
        }
    }

    impl PartialEq for Value {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (Self::Bool(left), Self::Bool(right)) => left == right,
                (Self::Number(left), Self::Number(right)) => left == right,
                (Self::Ref(left), Self::Ref(right)) => left.has_same_ptr_as(right),
                _ => core::mem::discriminant(self) == core::mem::discriminant(other),
            }
        }
    }

    impl Eq for Value {}
}

#[cfg(feature = "nan_boxing")]
mod value_inner {
    use std::mem;

    use crate::object::Ref;

    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;

    const NIL_TAG: u64 = 0b01;
    const FALSE_TAG: u64 = 0b10;
    const TRUE_TAG: u64 = 0b11;

    #[derive(Clone, Copy)]
    pub struct Value(pub u64);

    pub fn is_number(value: Value) -> bool {
        (value.0 & QNAN) != QNAN
    }

    pub fn is_bool(value: Value) -> bool {
        (value.0 | 1) == TRUE_VAL.0
    }

    pub fn is_nil(value: Value) -> bool {
        value.0 == NIL_VAL.0
    }

    pub fn is_obj(value: Value) -> bool {
        (value.0 & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT)
    }

    pub fn as_number(value: Value) -> f64 {
        value_to_num(value)
    }

    pub fn as_bool(value: Value) -> bool {
        value.0 == TRUE_VAL.0
    }

    pub fn as_obj(value: Value) -> *mut Ref<()> {
        unsafe { mem::transmute(value.0 & !(SIGN_BIT | QNAN)) }
    }

    pub fn number_val(num: f64) -> Value {
        num_to_value(num)
    }

    pub const NIL_VAL: Value = Value(QNAN | NIL_TAG);

    const TRUE_VAL: Value = Value(QNAN | TRUE_TAG);
    const FALSE_VAL: Value = Value(QNAN | FALSE_TAG);

    pub fn bool_val(b: bool) -> Value {
        if b {
            TRUE_VAL
        } else {
            FALSE_VAL
        }
    }

    pub fn obj_val(obj: *mut Ref<()>) -> Value {
        Value(SIGN_BIT | QNAN | unsafe { mem::transmute::<*mut Ref, u64>(obj) })
    }

    fn num_to_value(num: f64) -> Value {
        unsafe { mem::transmute(num) }
    }

    fn value_to_num(value: Value) -> f64 {
        unsafe { mem::transmute(value) }
    }

    pub unsafe fn print_value(value: Value) {
        match value {
            v if is_bool(v) => print!("{}", if as_bool(value) { "true" } else { "false" }),
            v if is_nil(v) => print!("nil"),
            v if is_number(v) => print!("{}", as_number(value)),
            v if is_obj(v) => print_object(value),
            _ => unreachable!("impossible value type"),
        }
    }

    pub unsafe fn values_equal(a: Value, b: Value) -> bool {
        if is_number(a) && is_number(b) {
            return as_number(a) == as_number(b);
        }
        a.0 == b.0
    }
}

pub use value_inner::*;
