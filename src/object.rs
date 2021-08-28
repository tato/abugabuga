use std::{ptr, slice, str};

use crate::{chunk::{init_chunk, Chunk}, memory::{gc_find_interned, gc_intern_string, gc_track_constant_for_chunk_or_strings_table, gc_track_object, gc_untrack_constant_for_chunk_or_strings_table, reallocate}, table::{init_table, Table}, value::{as_obj, is_obj, obj_val, print_value, Value, NIL_VAL}};

macro_rules! allocate_obj {
    ($t:ty, $obj_type:expr) => {
        allocate_object(std::mem::size_of::<$t>(), $obj_type) as *mut $t
    };
}

unsafe fn allocate_object(size: usize, ty: ObjType) -> *mut Obj {
    let object = reallocate(ptr::null_mut(), 0, size) as *mut Obj;
    (*object).ty = ty;
    (*object).is_marked = false;
    (*object).next = gc_track_object(object);

    #[cfg(feature = "debug_log_gc")]
    {
        println!("{:?} allocate {} for {:?}", object, size, ty);
    }

    object
}

unsafe fn allocate_string(chars: *mut u8, length: i32, hash: u32) -> *mut ObjString {
    let string = allocate_obj!(ObjString, ObjType::String);
    (*string).length = length;
    (*string).chars = chars;
    (*string).hash = hash;
    gc_track_constant_for_chunk_or_strings_table(obj_val(string as *mut Obj));
    gc_intern_string(string);
    gc_untrack_constant_for_chunk_or_strings_table();
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

pub unsafe fn _is_function(value: Value) -> bool {
    is_obj_type(value, ObjType::Function)
}

pub unsafe fn _is_native(value: Value) -> bool {
    is_obj_type(value, ObjType::Native)
}

pub unsafe fn is_string(value: Value) -> bool {
    is_obj_type(value, ObjType::String)
}

pub unsafe fn _is_closure(value: Value) -> bool {
    is_obj_type(value, ObjType::Closure)
}

pub unsafe fn is_class(value: Value) -> bool {
    is_obj_type(value, ObjType::Class)
}

pub unsafe fn is_instance(value: Value) -> bool {
    is_obj_type(value, ObjType::Instance)
}

pub unsafe fn _is_bound_method(value: Value) -> bool {
    is_obj_type(value, ObjType::BoundMethod)
}

pub unsafe fn is_list(value: Value) -> bool {
    is_obj_type(value, ObjType::List)
}

pub unsafe fn as_function(value: Value) -> *mut ObjFunction {
    as_obj(value) as *mut ObjFunction
}

pub unsafe fn as_native(value: Value) -> NativeFn {
    (*(as_obj(value) as *mut ObjNative)).function
}

pub unsafe fn as_string(value: Value) -> *mut ObjString {
    as_obj(value) as *mut ObjString
}

pub unsafe fn as_closure(value: Value) -> *mut ObjClosure {
    as_obj(value) as *mut ObjClosure
}

pub unsafe fn as_class(value: Value) -> *mut ObjClass {
    as_obj(value) as *mut ObjClass
}

pub unsafe fn as_instance(value: Value) -> *mut ObjInstance {
    as_obj(value) as *mut ObjInstance
}

pub unsafe fn as_bound_method(value: Value) -> *mut ObjBoundMethod {
    as_obj(value) as *mut ObjBoundMethod
}

pub unsafe fn as_list(value: Value) -> *mut ObjList {
    as_obj(value) as *mut ObjList
}

pub unsafe fn as_rs_str(value: Value) -> &'static str {
    let s = &*as_string(value);
    str::from_utf8_unchecked(slice::from_raw_parts(s.chars, s.length as usize))
}

unsafe fn is_obj_type(value: Value, ty: ObjType) -> bool {
    is_obj(value) && (*as_obj(value)).ty == ty
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObjType {
    BoundMethod,
    Class,
    Closure,
    Function,
    Instance,
    List,
    Native,
    String,
    Upvalue,
}

#[repr(C)]
pub struct Obj {
    pub ty: ObjType,
    pub is_marked: bool,
    pub next: *mut Obj,
}

#[repr(C)]
pub struct ObjFunction {
    pub obj: Obj,
    pub arity: i32,
    pub upvalue_count: i32,
    pub chunk: Chunk,
    pub name: *mut ObjString,
}

pub type NativeFn = unsafe fn(i32, *mut Value) -> Value;

#[repr(C)]
pub struct ObjNative {
    pub obj: Obj,
    pub function: NativeFn,
}

#[repr(C)]
pub struct ObjString {
    pub obj: Obj,
    pub length: i32,
    pub chars: *const u8,
    pub hash: u32,
}

#[repr(C)]
pub struct ObjUpvalue {
    pub obj: Obj,
    pub location: *mut Value,
    pub closed: Value,
    pub next: *mut ObjUpvalue,
}

#[repr(C)]
pub struct ObjClosure {
    pub obj: Obj,
    pub function: *mut ObjFunction,
    pub upvalues: *mut *mut ObjUpvalue,
    pub upvalue_count: i32,
}

#[repr(C)]
pub struct ObjClass {
    pub obj: Obj,
    pub name: *mut ObjString,
    pub methods: Table,
}

#[repr(C)]
pub struct ObjInstance {
    pub obj: Obj,
    pub class: *mut ObjClass,
    pub fields: Table,
}

#[repr(C)]
pub struct ObjBoundMethod {
    pub obj: Obj,
    pub receiver: Value,
    pub method: *mut ObjClosure,
}

#[repr(C)]
pub struct ObjList {
    pub obj: Obj,
    pub items: Vec<Value>,
}

pub unsafe fn new_list() -> *mut ObjList {
    let list = allocate_obj!(ObjList, ObjType::List);
    (*list).items = vec![];
    list
}

pub unsafe fn new_closure(function: *mut ObjFunction) -> *mut ObjClosure {
    let upvalues = allocate!(*mut ObjUpvalue, (*function).upvalue_count);
    for i in 0..(*function).upvalue_count {
        *upvalues.offset(i as isize) = ptr::null_mut();
    }

    let closure = allocate_obj!(ObjClosure, ObjType::Closure);
    (*closure).function = function;
    (*closure).upvalues = upvalues;
    (*closure).upvalue_count = (*function).upvalue_count;

    closure
}

pub unsafe fn new_function() -> *mut ObjFunction {
    let function = allocate_obj!(ObjFunction, ObjType::Function);
    (*function).arity = 0;
    (*function).upvalue_count = 0;
    (*function).name = ptr::null_mut();
    init_chunk(&mut (*function).chunk);
    function
}

pub unsafe fn new_native(function: NativeFn) -> *mut ObjNative {
    let native = allocate_obj!(ObjNative, ObjType::Native);
    (*native).function = function;
    native
}

pub unsafe fn new_class(name: *mut ObjString) -> *mut ObjClass {
    let class = allocate_obj!(ObjClass, ObjType::Class);
    (*class).name = name;
    init_table(&mut (*class).methods);
    class
}

pub unsafe fn new_instance(class: *mut ObjClass) -> *mut ObjInstance {
    let instance = allocate_obj!(ObjInstance, ObjType::Instance);
    (*instance).class = class;
    init_table(&mut (*instance).fields);
    instance
}

pub unsafe fn new_bound_method(receiver: Value, method: *mut ObjClosure) -> *mut ObjBoundMethod {
    let bound = allocate_obj!(ObjBoundMethod, ObjType::BoundMethod);
    (*bound).receiver = receiver;
    (*bound).method = method;
    bound
}

pub unsafe fn take_string(chars: *mut u8, length: i32) -> *mut ObjString {
    let hash = hash_string(chars, length);
    let interned = gc_find_interned(chars, length, hash);
    if interned != ptr::null_mut() {
        free_array!(u8, chars, length + 1);
        return interned;
    }
    allocate_string(chars, length, hash)
}

pub unsafe fn copy_string(chars: *const u8, length: i32) -> *mut ObjString {
    let hash = hash_string(chars, length);
    let interned = gc_find_interned(chars, length, hash);
    if interned != ptr::null_mut() {
        return interned;
    }
    let heap_chars = allocate!(u8, length + 1);
    ptr::copy_nonoverlapping(chars, heap_chars, length as usize);
    *heap_chars.offset(length as isize) = 0;
    allocate_string(heap_chars, length, hash)
}

pub unsafe fn new_upvalue(slot: *mut Value) -> *mut ObjUpvalue {
    let upvalue = allocate_obj!(ObjUpvalue, ObjType::Upvalue);
    (*upvalue).location = slot;
    (*upvalue).closed = NIL_VAL;
    (*upvalue).next = ptr::null_mut();
    upvalue
}

unsafe fn print_function(function: *mut ObjFunction) {
    if (*function).name == ptr::null_mut() {
        print!("<script>");
        return;
    }

    let name = &*(*function).name;
    print!(
        "<fn {}>",
        str::from_utf8_unchecked(slice::from_raw_parts(name.chars, name.length as usize))
    );
}

pub unsafe fn print_object(value: Value) {
    match obj_type(value) {
        ObjType::List => {
            let list = as_obj(value) as *mut ObjList;
            print!("[ ");
            for val in &(*list).items {
                print_value(*val);
                print!(", ");
            }
            print!("]");
        }
        ObjType::Instance => {
            print_object(obj_val((*as_instance(value)).class as *mut Obj));
            print!(" instance");
        }
        ObjType::Class => {
            print_object(obj_val((*as_class(value)).name as *mut Obj));
        }
        ObjType::Function => print_function(as_function(value)),
        ObjType::Native => print!("<native fn>"),
        ObjType::String => print!("{}", as_rs_str(value)),
        ObjType::Upvalue => print!("upvalue"),
        ObjType::Closure => print_function((*as_closure(value)).function),
        ObjType::BoundMethod => print_function((*(*as_bound_method(value)).method).function),
    }
}
