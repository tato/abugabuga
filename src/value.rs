use std::ptr;

pub type Value = f32;

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

pub fn print_value(value: Value) {
    print!("{}", value);
}