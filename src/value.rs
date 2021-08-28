use std::ptr;

use crate::object::print_object;

#[cfg(not(feature = "nan_boxing"))]
mod value_inner {
    use crate::object::Obj;

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
        pub obj: *mut Obj,
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

    pub unsafe fn as_bool(value: Value) -> bool {
        value.val.boolean
    }

    pub unsafe fn as_number(value: Value) -> f64 {
        value.val.number
    }

    pub unsafe fn as_obj(value: Value) -> *mut Obj {
        value.val.obj
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

    pub fn obj_val(value: *mut Obj) -> Value {
        Value {
            ty: ValueType::Obj,
            val: ValueValue { obj: value },
        }
    }
}

#[cfg(feature = "nan_boxing")]
mod value_inner {
    use std::mem;

    use crate::object::Obj;

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

    pub fn as_obj(value: Value) -> *mut Obj {
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

    pub fn obj_val(obj: *mut Obj) -> Value {
        Value(SIGN_BIT | QNAN | unsafe { mem::transmute::<*mut Obj, u64>(obj) })
    }

    fn num_to_value(num: f64) -> Value {
        unsafe { mem::transmute(num) }
    }

    fn value_to_num(value: Value) -> f64 {
        unsafe { mem::transmute(value) }
    }
}

pub use value_inner::*;

pub struct ValueArray {
    pub capacity: i32,
    pub count: i32,
    pub values: *mut Value,
}

pub unsafe fn init_value_array(array: *mut ValueArray) {
    let array = &mut *array;
    array.count = 0;
    array.capacity = 0;
    array.values = ptr::null_mut();
}

pub unsafe fn write_value_array(array: *mut ValueArray, value: Value) {
    let array = &mut *array;

    if array.capacity < array.count + 1 {
        let old_capacity = array.capacity;
        array.capacity = grow_capacity!(old_capacity);
        array.values = grow_array!(Value, array.values, old_capacity, array.capacity);
    }

    *array.values.offset(array.count as isize) = value;
    array.count += 1;
}

pub unsafe fn free_value_array(array: *mut ValueArray) {
    {
        let array = &mut *array;
        free_array!(Value, array.values, array.capacity);
    }
    init_value_array(array);
}

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
        ValueType::Obj => as_obj(a) == as_obj(b),
    }
}

#[cfg(feature = "nan_boxing")]
pub unsafe fn values_equal(a: Value, b: Value) -> bool {
    if is_number(a) && is_number(b) {
        return as_number(a) == as_number(b);
    }
    a.0 == b.0
}
