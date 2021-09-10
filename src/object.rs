use std::{mem, ptr, str};

use crate::{
    array::{Array, Table},
    chunk::{init_chunk, Chunk},
    memory::{
        gc_find_interned, gc_intern_string, gc_track_constant_for_chunk_or_strings_table,
        gc_track_object, gc_untrack_constant_for_chunk_or_strings_table, reallocate,
    },
    value::{as_obj, is_obj, obj_val, print_value, Value, NIL_VAL},
};

// TODO: this goes in gc.rs
unsafe fn allocate_object<T>(ty: ObjType) -> *mut Obj<T> {
    let size = mem::size_of::<Obj<T>>();
    let align = mem::align_of::<Obj<T>>();

    let object = reallocate(ptr::null_mut(), 0, size, align) as *mut Obj<T>;
    (*object).ty = ty;
    (*object).is_marked = false;
    (*object).next = gc_track_object(object as *mut Obj<()>);

    #[cfg(feature = "debug_log_gc")]
    {
        println!("{:?} allocate {} for {:?}", object, size, ty);
    }

    object
}

unsafe fn allocate_string(chars: Array<u8>, hash: u32) -> *mut Obj<ObjString> {
    let string = allocate_object::<ObjString>(ObjType::String);
    (*string).value.chars = chars;
    (*string).value.hash = hash;
    gc_track_constant_for_chunk_or_strings_table(obj_val(string as *mut Obj<()>));
    gc_intern_string(string);
    gc_untrack_constant_for_chunk_or_strings_table();
    string
}

unsafe fn hash_string(key: &[u8]) -> u32 {
    let mut hash = 2166136261u32;
    for &byte in key {
        hash = hash ^ byte as u32;
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

pub unsafe fn as_function(value: Value) -> *mut Obj<ObjFunction> {
    as_obj(value) as *mut Obj<ObjFunction>
}

pub unsafe fn as_native(value: Value) -> NativeFn {
    (*(as_obj(value) as *mut Obj<ObjNative>)).value.function
}

pub unsafe fn as_string(value: Value) -> *mut Obj<ObjString> {
    as_obj(value) as *mut Obj<ObjString>
}

pub unsafe fn as_closure(value: Value) -> *mut Obj<ObjClosure> {
    as_obj(value) as *mut Obj<ObjClosure>
}

pub unsafe fn as_class(value: Value) -> *mut Obj<ObjClass> {
    as_obj(value) as *mut Obj<ObjClass>
}

pub unsafe fn as_instance(value: Value) -> *mut Obj<ObjInstance> {
    as_obj(value) as *mut Obj<ObjInstance>
}

pub unsafe fn as_bound_method(value: Value) -> *mut Obj<ObjBoundMethod> {
    as_obj(value) as *mut Obj<ObjBoundMethod>
}

pub unsafe fn as_list(value: Value) -> *mut Obj<ObjList> {
    as_obj(value) as *mut Obj<ObjList>
}

pub unsafe fn as_rs_str(value: Value) -> &'static str {
    let s = &*as_string(value);
    str::from_utf8_unchecked(&s.value.chars[..])
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
pub struct Obj<T> {
    pub ty: ObjType,
    pub is_marked: bool,
    pub next: *mut Obj<()>,
    pub value: T,
}

#[repr(C)]
pub struct ObjFunction {
    pub arity: i32,
    pub upvalue_count: i32,
    pub chunk: Chunk,
    pub name: *mut Obj<ObjString>,
}

pub type NativeFn = unsafe fn(i32, *mut Value) -> Value;

#[repr(C)]
pub struct ObjNative {
    pub function: NativeFn,
}

#[repr(C)]
pub struct ObjString {
    pub chars: Array<u8>,
    pub hash: u32,
}

#[repr(C)]
pub struct ObjUpvalue {
    pub location: *mut Value,
    pub closed: Value,
    pub next: *mut Obj<ObjUpvalue>,
}

#[repr(C)]
pub struct ObjClosure {
    pub function: *mut Obj<ObjFunction>,
    pub upvalues: Array<*mut Obj<ObjUpvalue>>,
}

#[repr(C)]
pub struct ObjClass {
    pub name: *mut Obj<ObjString>,
    pub methods: Table,
}

#[repr(C)]
pub struct ObjInstance {
    pub class: *mut Obj<ObjClass>,
    pub fields: Table,
}

#[repr(C)]
pub struct ObjBoundMethod {
    pub receiver: Value,
    pub method: *mut Obj<ObjClosure>,
}

#[repr(C)]
pub struct ObjList {
    pub items: Vec<Value>,
}

pub unsafe fn new_list() -> *mut Obj<ObjList> {
    let list = allocate_object::<ObjList>(ObjType::List);
    (*list).value.items = vec![];
    list
}

pub unsafe fn new_closure(function: *mut Obj<ObjFunction>) -> *mut Obj<ObjClosure> {
    let mut upvalues = Array::with_capacity((*function).value.upvalue_count as usize);
    for _i in 0..(*function).value.upvalue_count {
        upvalues.write(ptr::null_mut());
    }

    let closure = allocate_object::<ObjClosure>(ObjType::Closure);
    (*closure).value.function = function;
    (*closure).value.upvalues = upvalues;

    closure
}

pub unsafe fn new_function() -> *mut Obj<ObjFunction> {
    let function = allocate_object::<ObjFunction>(ObjType::Function);
    (*function).value.arity = 0;
    (*function).value.upvalue_count = 0;
    (*function).value.name = ptr::null_mut();
    init_chunk(&mut (*function).value.chunk);
    function
}

pub unsafe fn new_native(function: NativeFn) -> *mut Obj<ObjNative> {
    let native = allocate_object::<ObjNative>(ObjType::Native);
    (*native).value.function = function;
    native
}

pub unsafe fn new_class(name: *mut Obj<ObjString>) -> *mut Obj<ObjClass> {
    let class = allocate_object::<ObjClass>(ObjType::Class);
    (*class).value.name = name;
    (*class).value.methods = Table::new();
    class
}

pub unsafe fn new_instance(class: *mut Obj<ObjClass>) -> *mut Obj<ObjInstance> {
    let instance = allocate_object::<ObjInstance>(ObjType::Instance);
    (*instance).value.class = class;
    (*instance).value.fields = Table::new();
    instance
}

pub unsafe fn new_bound_method(
    receiver: Value,
    method: *mut Obj<ObjClosure>,
) -> *mut Obj<ObjBoundMethod> {
    let bound = allocate_object::<ObjBoundMethod>(ObjType::BoundMethod);
    (*bound).value.receiver = receiver;
    (*bound).value.method = method;
    bound
}

pub unsafe fn take_string(mut chars: Array<u8>) -> *mut Obj<ObjString> {
    let hash = hash_string(&chars[..]);
    let interned = gc_find_interned(&chars[..], hash);
    if interned != ptr::null_mut() {
        chars.free();
        return interned;
    }
    allocate_string(chars, hash)
}

pub unsafe fn copy_string(chars: &[u8]) -> *mut Obj<ObjString> {
    let hash = hash_string(chars);
    let interned = gc_find_interned(chars, hash);
    if interned != ptr::null_mut() {
        return interned;
    }
    let char_array = Array::from(chars);
    allocate_string(char_array, hash)
}

pub unsafe fn new_upvalue(slot: *mut Value) -> *mut Obj<ObjUpvalue> {
    let upvalue = allocate_object::<ObjUpvalue>(ObjType::Upvalue);
    (*upvalue).value.location = slot;
    (*upvalue).value.closed = NIL_VAL;
    (*upvalue).next = ptr::null_mut(); // TODO: huh?
    upvalue
}

unsafe fn print_function(function: *mut Obj<ObjFunction>) {
    if (*function).value.name == ptr::null_mut() {
        print!("<script>");
        return;
    }

    let name = &*(*function).value.name;
    print!("<fn {}>", str::from_utf8_unchecked(&name.value.chars[..]));
}

pub unsafe fn print_object(value: Value) {
    match obj_type(value) {
        ObjType::List => {
            let list = as_obj(value) as *mut Obj<ObjList>;
            print!("[ ");
            for val in &(*list).value.items {
                print_value(*val);
                print!(", ");
            }
            print!("]");
        }
        ObjType::Instance => {
            print_object(obj_val((*as_instance(value)).value.class as *mut Obj<()>));
            print!(" instance");
        }
        ObjType::Class => {
            print_object(obj_val((*as_class(value)).value.name as *mut Obj<()>));
        }
        ObjType::Function => print_function(as_function(value)),
        ObjType::Native => print!("<native fn>"),
        ObjType::String => print!("{}", as_rs_str(value)),
        ObjType::Upvalue => print!("upvalue"),
        ObjType::Closure => print_function((*as_closure(value)).value.function),
        ObjType::BoundMethod => {
            print_function((*(*as_bound_method(value)).value.method).value.function)
        }
    }
}
