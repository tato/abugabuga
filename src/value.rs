use crate::object::print_object;

#[cfg(not(feature = "nan_boxing"))]
mod value_inner {
    use crate::memory::{Ref};

    
    #[repr(u8)]
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum ValueType {
        Bool,
        Nil,
        Number,
        Obj,
    }

    #[derive(Clone, Copy)]
    pub union ValueValue {
        pub boolean: bool,
        pub number: f64,
        pub obj: Ref<()>,
    }

    #[derive(Clone, Copy)]
    pub struct Value {
        pub ty: ValueType,
        pub val: ValueValue,
    }

    pub fn is_bool(value: Value) -> bool {
        value.ty == ValueType::Bool
    }

    pub fn is_nil(value: Value) -> bool {
        value.ty == ValueType::Nil
    }

    pub fn is_number(value: Value) -> bool {
        value.ty == ValueType::Number
    }

    pub fn is_obj(value: Value) -> bool {
        value.ty == ValueType::Obj
    }

    pub fn as_bool(value: Value) -> bool {
        unsafe { value.val.boolean }
    }

    pub fn as_number(value: Value) -> f64 {
        unsafe { value.val.number }
    }

    pub fn as_erased_ref(value: Value) -> Ref<()> {
        unsafe { value.val.obj }
    }

    pub fn bool_val(value: bool) -> Value {
        Value {
            ty: ValueType::Bool,
            val: ValueValue { boolean: value },
        }
    }

    pub const NIL_VAL: Value = Value {
        ty: ValueType::Nil,
        val: ValueValue { number: 0.0 },
    };

    pub fn number_val(value: f64) -> Value {
        Value {
            ty: ValueType::Number,
            val: ValueValue { number: value },
        }
    }

    pub fn obj_val<T>(value: Ref<T>) -> Value {
        Value {
            ty: ValueType::Obj,
            val: ValueValue {
                obj: value.type_erased(),
            },
        }
    }
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
}

pub use value_inner::*;

#[cfg(not(feature = "nan_boxing"))]
pub unsafe fn print_value(value: Value) {
    match value.ty {
        ValueType::Bool => print!("{}", if as_bool(value) { "true" } else { "false" }),
        ValueType::Nil => print!("nil"),
        ValueType::Number => print!("{}", as_number(value)),
        ValueType::Obj => print_object(value),
    }
}

#[cfg(feature = "nan_boxing")]
pub unsafe fn print_value(value: Value) {
    match value {
        v if is_bool(v) => print!("{}", if as_bool(value) { "true" } else { "false" }),
        v if is_nil(v) => print!("nil"),
        v if is_number(v) => print!("{}", as_number(value)),
        v if is_obj(v) => print_object(value),
        _ => unreachable!("impossible value type"),
    }
}

#[cfg(not(feature = "nan_boxing"))]
pub unsafe fn values_equal(a: Value, b: Value) -> bool {
    if a.ty != b.ty {
        return false;
    }
    match a.ty {
        ValueType::Bool => as_bool(a) == as_bool(b),
        ValueType::Nil => true,
        ValueType::Number => as_number(a) == as_number(b),
        ValueType::Obj => as_erased_ref(a).same_ptr(&as_erased_ref(b)),
    }
}

#[cfg(feature = "nan_boxing")]
pub unsafe fn values_equal(a: Value, b: Value) -> bool {
    if is_number(a) && is_number(b) {
        return as_number(a) == as_number(b);
    }
    a.0 == b.0
}
