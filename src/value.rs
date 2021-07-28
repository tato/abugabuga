use std::ptr;

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ValueType {
    Bool,
    Nil,
    Number,
}

#[derive(Clone, Copy)]
pub union ValueValue {
    boolean: bool,
    number: f64,
}

#[derive(Clone, Copy)]
pub struct Value {
    ty: ValueType,
    val: ValueValue,
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

pub unsafe fn as_bool(value: Value) -> bool {
    value.val.boolean
}

pub unsafe fn as_number(value: Value) -> f64 {
    value.val.number
}

pub fn bool_val(value: bool) -> Value {
    Value{ ty: ValueType::Bool, val: ValueValue{ boolean: value }}
}

pub const fn nil_val() -> Value {
    Value{ ty: ValueType::Nil, val: ValueValue{ number: 0.0 }}
}

pub fn number_val(value: f64) -> Value {
    Value{ ty: ValueType::Number, val: ValueValue{ number: value }}
}
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

pub unsafe fn print_value(value: Value) {
    match value.ty {
        ValueType::Bool => print!("{}", if as_bool(value) { "true" } else { "false" }),
        ValueType::Nil => print!("nil"),
        ValueType::Number => print!("{}", as_number(value)),
    }
}

pub unsafe fn values_equal(a: Value, b: Value) -> bool {
    if a.ty != b.ty {
        return false;
    }
    match a.ty {
        ValueType::Bool => as_bool(a) == as_bool(b),
        ValueType::Nil => true,
        ValueType::Number => as_number(a) == as_number(b),
        _ => false,
    }
}