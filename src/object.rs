use std::{ptr, slice, str};

use crate::{memory::reallocate, value::{Value, as_obj, is_obj}, vm::vm};

macro_rules! allocate_obj {
    ($t:ty, $obj_type:expr) => {
        allocate_object(
            std::mem::size_of::<$t>(),
            $obj_type,
        ) as *mut $t
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

unsafe fn allocate_string(chars: *mut u8, length: i32) -> *mut ObjString {
    let string = allocate_obj!(ObjString, ObjType::String);
    {
        let string = &mut *string;
        string.length = length;
        string.chars = chars;
    }
    string
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
}

pub unsafe fn take_string(chars: *mut u8, length: i32) -> *mut ObjString {
    allocate_string(chars, length)
}

pub unsafe fn copy_string(chars: *const u8, length: i32) -> *mut ObjString {
    let heap_chars = allocate!(u8, length + 1);
    ptr::copy_nonoverlapping(chars, heap_chars, length as usize);
    *heap_chars.offset(length as isize) = 0;
    allocate_string(heap_chars, length)
}

pub unsafe fn print_object(value: Value) {
    match obj_type(value) {
        ObjType::String => print!("{}", as_rs_str(value)),
    }
}