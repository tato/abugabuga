use std::str;

use crate::{array::{Array, Table}, chunk::{init_chunk, Chunk}, memory::{
        allocate_object,ObjType, Ref,
    }, value::Value};

unsafe fn allocate_string(chars: Array<u8>, hash: u32) -> Ref<ObjString> {
    let mut string = allocate_object::<ObjString>(ObjType::String);
    string.value_mut().chars = chars;
    string.value_mut().hash = hash;
    todo!("interner.intern(string);");
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

pub fn obj_type(value: Value) -> ObjType {
    value.as_erased_ref().expect("Value should be a Ref").ty()
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
    value.as_erased_ref().unwrap().force_into()
}

pub unsafe fn as_native(value: Value) -> NativeFn {
    let rf: Ref<ObjNative> = value.as_erased_ref().unwrap().force_into();
    rf.value().function
}

pub unsafe fn as_string(value: Value) -> Ref<ObjString> {
    value.as_erased_ref().unwrap().force_into()
}

pub unsafe fn as_closure(value: Value) -> Ref<ObjClosure> {
    value.as_erased_ref().unwrap().force_into()
}

pub unsafe fn as_class(value: Value) -> Ref<ObjClass> {
    value.as_erased_ref().unwrap().force_into()
}

pub unsafe fn as_instance(value: Value) -> Ref<ObjInstance> {
    value.as_erased_ref().unwrap().force_into()
}

pub unsafe fn as_bound_method(value: Value) -> Ref<ObjBoundMethod> {
    value.as_erased_ref().unwrap().force_into()
}

pub unsafe fn as_list(value: Value) -> Ref<ObjList> {
    value.as_erased_ref().unwrap().force_into()
}

unsafe fn is_obj_type(value: Value, ty: ObjType) -> bool {
    value.as_erased_ref().is_some() && value.as_erased_ref().unwrap().ty() == ty
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
        upvalues.append(None);
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

pub unsafe fn new_bound_method(receiver: Value, method: Ref<ObjClosure>) -> Ref<ObjBoundMethod> {
    let mut bound = allocate_object::<ObjBoundMethod>(ObjType::BoundMethod);
    bound.value_mut().receiver = receiver;
    bound.value_mut().method = method;
    bound
}

pub unsafe fn take_string(mut chars: Array<u8>) -> Ref<ObjString> {
    let hash = hash_string(&chars[..]);
    let interned = todo!("interner.find(&chars[..], hash);");
    if let Some(interned) = interned {
        chars.free();
        return interned;
    }
    allocate_string(chars, hash)
}

pub unsafe fn copy_string(chars: &[u8]) -> Ref<ObjString> {
    let hash = hash_string(chars);
    let interned = todo!("interner.find(chars, hash);");
    if let Some(interned) = interned {
        return interned;
    }
    let char_array = Array::from(chars);
    allocate_string(char_array, hash)
}

pub unsafe fn new_upvalue(slot: *mut Value) -> Ref<ObjUpvalue> {
    let mut upvalue = allocate_object::<ObjUpvalue>(ObjType::Upvalue);
    upvalue.value_mut().location = slot;
    upvalue.value_mut().closed = Value::NIL;
    upvalue.value_mut().next = None;
    upvalue
}

fn print_function(function: Ref<ObjFunction>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match function.value().name {
        None => write!(f, "<script>"),
        Some(name) => {
            write!(
                f,
                "<fn {}>",
                str::from_utf8(&name.value().chars[..]).expect("Function name should be utf-8")
            )
        }
    }
}

pub fn print_object(value: &Value, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let value = *value;
    match obj_type(value) {
        ObjType::List => {
            let list = unsafe { as_list(value) };
            write!(f, "[ ")?;
            for val in &list.value().items {
                write!(f, "{}", *val)?;
                write!(f, ", ")?;
            }
            write!(f, "]")
        }
        ObjType::Instance => {
            let ins = unsafe { as_instance(value) };
            print_object(&ins.value().class.into(), f)?;
            write!(f, " instance")
        }
        ObjType::Class => {
            let cl = unsafe { as_class(value) };
            print_object(&cl.value().name.into(), f)
        }
        ObjType::Function => {
            let fun = unsafe { as_function(value) };
            print_function(fun, f)
        }
        ObjType::Native => write!(f, "<native fn>"),
        ObjType::String => {
            let st = unsafe { as_string(value) };
            write!(
                f,
                "{}",
                str::from_utf8(&st.value().chars[..]).expect("String object should contain Utf-8")
            )
        }
        ObjType::Upvalue => write!(f, "upvalue"),
        ObjType::Closure => {
            let clos = unsafe { as_closure(value) };
            print_function(clos.value().function, f)
        }
        ObjType::BoundMethod => {
            let bm = unsafe { as_bound_method(value) };
            print_function(bm.value().method.value().function, f)
        }
    }
}
