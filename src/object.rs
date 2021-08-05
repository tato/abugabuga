use std::{ptr, slice, str};

use crate::{
    memory::reallocate,
    table::{table_find_string, table_set},
    value::{as_obj, is_obj, nil_val, Value},
    vm::vm,
};

macro_rules! allocate_obj {
    ($t:ty, $obj_type:expr) => {
        allocate_object(std::mem::size_of::<$t>(), $obj_type) as *mut $t
    };
}

unsafe fn allocate_object(size: usize, ty: ObjType) -> *mut Obj {
    let obj = reallocate(ptr::null_mut(), 0, size) as *mut Obj;
    {
        let obj = &mut *obj;
        obj.ty = ty;

        obj.next = vm.objects;
        vm.objects = obj;
    }
    obj
}

unsafe fn allocate_string(chars: *mut u8, length: i32, hash: u32) -> *mut ObjString {
    let string = allocate_obj!(ObjString, ObjType::String);
    {
        let string = &mut *string;
        string.length = length;
        string.chars = chars;
        string.hash = hash;
        table_set(&mut vm.strings, string, nil_val());
    }
    string
}

unsafe fn hash_string(key: *const u8, length: i32) -> u32 {
    let mut hash = 2166136261u32;
    for i in 0..length {
        hash = hash ^ *key.offset(i as isize) as u32;
        hash = hash.wrapping_mul(16777619);
    }
    hash
}

pub unsafe fn obj_type(value: Value) -> ObjType {
    (*as_obj(value)).ty
}

pub unsafe fn is_string(value: Value) -> bool {
    is_obj_type(value, ObjType::String)
}

pub unsafe fn as_string(value: Value) -> *mut ObjString {
    as_obj(value) as *mut ObjString
}

pub unsafe fn as_rs_str(value: Value) -> &'static str {
    let s = &*as_string(value);
    str::from_utf8_unchecked(slice::from_raw_parts(s.chars, s.length as usize))
}

unsafe fn is_obj_type(value: Value, ty: ObjType) -> bool {
    is_obj(value) && (*as_obj(value)).ty == ty
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ObjType {
    String,
}

pub struct Obj {
    pub ty: ObjType,
    pub next: *mut Obj,
}

pub struct ObjString {
    pub obj: Obj,
    pub length: i32,
    pub chars: *const u8,
    pub hash: u32,
}

pub unsafe fn take_string(chars: *mut u8, length: i32) -> *mut ObjString {
    let hash = hash_string(chars, length);
    let interned = table_find_string(&mut vm.strings, chars, length, hash);
    if interned != ptr::null_mut() {
        free_array!(u8, chars, length + 1);
        return interned;
    }
    allocate_string(chars, length, hash)
}

pub unsafe fn copy_string(chars: *const u8, length: i32) -> *mut ObjString {
    let hash = hash_string(chars, length);
    let interned = table_find_string(&mut vm.strings, chars, length, hash);
    if interned != ptr::null_mut() {
        return interned;
    }
    let heap_chars = allocate!(u8, length + 1);
    ptr::copy_nonoverlapping(chars, heap_chars, length as usize);
    *heap_chars.offset(length as isize) = 0;
    allocate_string(heap_chars, length, hash)
}

pub unsafe fn print_object(value: Value) {
    match obj_type(value) {
        ObjType::String => print!("{}", as_rs_str(value)),
    }
}
