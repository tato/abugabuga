use std::{mem, ptr::{self, NonNull}, str};

use crate::{
    array::{Array, Table},
    chunk::{init_chunk, Chunk},
    memory::{
        gc_find_interned, gc_intern_string, gc_track_constant_for_chunk_or_strings_table,
        gc_track_object, gc_untrack_constant_for_chunk_or_strings_table, reallocate,
    },
    value::{as_obj_header, is_obj, obj_val, print_value, Value, NIL_VAL},
};

// TODO: this goes in gc.rs
unsafe fn allocate_object<T>(ty: ObjType) -> Ref<T> {
    let size = mem::size_of::<RefStorage<T>>();
    let align = mem::align_of::<RefStorage<T>>();

    let object = reallocate(ptr::null_mut(), 0, size, align) as *mut RefStorage<T>;
    assert!(object != ptr::null_mut(), "Probably unnecesary sanity check.");

    let header = &mut (*object).header;
    header.ty = ty;
    header.is_marked = false;
    header.next = gc_track_object(header);

    #[cfg(feature = "debug_log_gc")]
    {
        println!("{:?} allocate {} for {:?}", object, size, ty);
    }

    Ref{ inner: mem::transmute(object) }
}

unsafe fn allocate_string(chars: Array<u8>, hash: u32) -> Ref<ObjString> {
    let mut string = allocate_object::<ObjString>(ObjType::String);
    string.value_mut().chars = chars;
    string.value_mut().hash = hash;
    gc_track_constant_for_chunk_or_strings_table(obj_val(string));
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
    (*as_obj_header(value)).ty
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

pub unsafe fn as_function(value: Value) -> Ref<ObjFunction> {
    Ref::from_header(as_obj_header(value))
}

pub unsafe fn as_native(value: Value) -> NativeFn {
    let rf: Ref<ObjNative> = Ref::from_header(as_obj_header(value));
    rf.value().function
}

pub unsafe fn as_string(value: Value) -> Ref<ObjString> {
    Ref::from_header(as_obj_header(value))
}

pub unsafe fn as_closure(value: Value) -> Ref<ObjClosure> {
    Ref::from_header(as_obj_header(value))
}

pub unsafe fn as_class(value: Value) -> Ref<ObjClass> {
    Ref::from_header(as_obj_header(value))
}

pub unsafe fn as_instance(value: Value) -> Ref<ObjInstance> {
    Ref::from_header(as_obj_header(value))
}

pub unsafe fn as_bound_method(value: Value) -> Ref<ObjBoundMethod> {
    Ref::from_header(as_obj_header(value))
}

pub unsafe fn as_list(value: Value) -> Ref<ObjList> {
    Ref::from_header(as_obj_header(value))
}

unsafe fn is_obj_type(value: Value, ty: ObjType) -> bool {
    is_obj(value) && (*as_obj_header(value)).ty == ty
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
pub struct Ref<T> {
    inner: ptr::NonNull<RefStorage<T>>
}

impl<T> std::clone::Clone for Ref<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}
impl<T> std::marker::Copy for Ref<T> {}


impl<T> Ref<T> {
    pub fn value(&self) -> &T {
        unsafe {
            &self.inner.as_ref().value
        }
    }

    pub fn value_mut(&mut self) -> &mut T {
        unsafe {
            &mut self.inner.as_mut().value
        }
    }

    pub fn header(&self) -> &GcHeader {
        unsafe { &self.inner.as_ref().header }
    }

    pub fn header_mut(&mut self) -> &mut GcHeader {
        // MOST DEFINITELY NOT THREAD SAFE
        unsafe { &mut self.inner.as_mut().header }
    }

    // TODO: OBLITERATE
    pub fn same_ptr<X>(&self, other: &Ref<X>) -> bool {
        self.inner.as_ptr() as usize == other.inner.as_ptr() as usize
    }

    pub unsafe fn from_header(header: *mut GcHeader) -> Ref<T> {
        Ref { inner: mem::transmute(header) }
    }

    pub const fn dangling() -> Ref<T> {
        Ref { inner: NonNull::dangling() }
    }
}

#[repr(C)]
pub struct RefStorage<T> {
    header: GcHeader,
    value: T,
}

#[repr(C)]
pub struct GcHeader {
    pub ty: ObjType,
    pub is_marked: bool,
    pub next: *mut GcHeader,
}

#[repr(C)]
pub struct ObjFunction {
    pub arity: i32,
    pub upvalue_count: i32,
    pub chunk: Chunk,
    pub name: Option<Ref<ObjString>>,
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
    pub next: Option<Ref<ObjUpvalue>>,
}

#[repr(C)]
pub struct ObjClosure {
    pub function: Ref<ObjFunction>,
    pub upvalues: Array<Option<Ref<ObjUpvalue>>>,
}

#[repr(C)]
pub struct ObjClass {
    pub name: Ref<ObjString>,
    pub methods: Table,
}

#[repr(C)]
pub struct ObjInstance {
    pub class: Ref<ObjClass>,
    pub fields: Table,
}

#[repr(C)]
pub struct ObjBoundMethod {
    pub receiver: Value,
    pub method: Ref<ObjClosure>,
}

#[repr(C)]
pub struct ObjList {
    pub items: Vec<Value>,
}

pub unsafe fn new_list() -> Ref<ObjList> {
    let mut list = allocate_object::<ObjList>(ObjType::List);
    list.value_mut().items = vec![];
    list
}

pub unsafe fn new_closure(function: Ref<ObjFunction>) -> Ref<ObjClosure> {
    let upvalue_count = function.value().upvalue_count;
    let mut upvalues = Array::with_capacity(upvalue_count as usize);
    for _i in 0..upvalue_count {
        upvalues.write(None);
    }

    let mut closure = allocate_object::<ObjClosure>(ObjType::Closure);
    closure.value_mut().function = function;
    closure.value_mut().upvalues = upvalues;

    closure
}

pub unsafe fn new_function() -> Ref<ObjFunction> {
    let mut function = allocate_object::<ObjFunction>(ObjType::Function);
    function.value_mut().arity = 0;
    function.value_mut().upvalue_count = 0;
    function.value_mut().name = None;
    init_chunk(&mut function.value_mut().chunk);
    function
}

pub unsafe fn new_native(function: NativeFn) -> Ref<ObjNative> {
    let mut native = allocate_object::<ObjNative>(ObjType::Native);
    native.value_mut().function = function;
    native
}

pub unsafe fn new_class(name: Ref<ObjString>) -> Ref<ObjClass> {
    let mut class = allocate_object::<ObjClass>(ObjType::Class);
    class.value_mut().name = name;
    class.value_mut().methods = Table::new();
    class
}

pub unsafe fn new_instance(class: Ref<ObjClass>) -> Ref<ObjInstance> {
    let mut instance = allocate_object::<ObjInstance>(ObjType::Instance);
    instance.value_mut().class = class;
    instance.value_mut().fields = Table::new();
    instance
}

pub unsafe fn new_bound_method(
    receiver: Value,
    method: Ref<ObjClosure>,
) -> Ref<ObjBoundMethod> {
    let mut bound = allocate_object::<ObjBoundMethod>(ObjType::BoundMethod);
    bound.value_mut().receiver = receiver;
    bound.value_mut().method = method;
    bound
}

pub unsafe fn take_string(mut chars: Array<u8>) -> Ref<ObjString> {
    let hash = hash_string(&chars[..]);
    let interned = gc_find_interned(&chars[..], hash);
    if let Some(interned) = interned {
        chars.free();
        return interned;
    }
    allocate_string(chars, hash)
}

pub unsafe fn copy_string(chars: &[u8]) -> Ref<ObjString> {
    let hash = hash_string(chars);
    let interned = gc_find_interned(chars, hash);
    if let Some(interned) = interned {
        return interned;
    }
    let char_array = Array::from(chars);
    allocate_string(char_array, hash)
}

pub unsafe fn new_upvalue(slot: *mut Value) -> Ref<ObjUpvalue> {
    let mut upvalue = allocate_object::<ObjUpvalue>(ObjType::Upvalue);
    upvalue.value_mut().location = slot;
    upvalue.value_mut().closed = NIL_VAL;
    upvalue.value_mut().next = None;
    upvalue
}

unsafe fn print_function(function: Ref<ObjFunction>) {

    match function.value().name {
        None => print!("<script>"),
        Some(name) => {
            print!("<fn {}>", str::from_utf8_unchecked(&name.value().chars[..]));
        }
    }
}

pub unsafe fn print_object(value: Value) {
    match obj_type(value) {
        ObjType::List => {
            let list = as_list(value);
            print!("[ ");
            for val in &list.value().items {
                print_value(*val);
                print!(", ");
            }
            print!("]");
        }
        ObjType::Instance => {
            print_object(obj_val(as_instance(value).value().class));
            print!(" instance");
        }
        ObjType::Class => {
            print_object(obj_val(as_class(value).value().name));
        }
        ObjType::Function => print_function(as_function(value)),
        ObjType::Native => print!("<native fn>"),
        ObjType::String => print!("{}", str::from_utf8_unchecked(&as_string(value).value().chars[..])),
        ObjType::Upvalue => print!("upvalue"),
        ObjType::Closure => print_function(as_closure(value).value().function),
        ObjType::BoundMethod => {
            print_function(as_bound_method(value).value().method.value().function)
        }
    }
}
