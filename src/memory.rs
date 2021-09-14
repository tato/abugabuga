use std::{
    alloc,
    ffi::c_void,
    mem,
    ptr::{self, NonNull},
    vec,
};

use crate::{
    array::{Array, Table},
    chunk::free_chunk,
    compiler::Parser,
    object::{
        ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjList, ObjNative,
        ObjString, ObjUpvalue,
    },
    value::Value,
    vm::VM,
};

#[cfg(feature = "debug_log_gc")]
use crate::value::{obj_val, print_value};

pub fn allocate<T>(count: usize) -> *mut T {
    crate::memory::reallocate(
        std::ptr::null_mut(),
        0,
        std::mem::size_of::<T>() * count as usize,
        std::mem::align_of::<T>(),
    ) as *mut T
}

fn free<T>(mut rf: Ref<T>) {
    reallocate(
        rf.header_mut() as *mut GcHeader as *mut std::ffi::c_void,
        std::mem::size_of::<RefStorage<T>>(),
        0,
        std::mem::align_of::<RefStorage<T>>(),
    );
}

// TODO: WHERE AND HOW SHOULD THE POSSIBLE TYPES BE DEFINED?
// ALL THE ObjX STRUCTS ARE DEFINED IN object.rs, IT DOESN'T MAKE SENSE
// THAT THIS ENUM IS DEFINED HERE.
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
    inner: ptr::NonNull<RefStorage<T>>,
}

// TODO: IS CLONE/COPY SOUND FOR THIS TYPE?
// STUFF LIKE AN Rc DOESN'T HAVE COPY BECAUSE THEY NEED
// TO INCREASE THE REFERENCE COUNT AND STUFF... HERE IT'S JUST A POINTER
// BUT I THINK WITH THREADS, IMPLEMENTING Copy MIGHT ALLOW SHARING WITHOUT
// LIMITS AND THAT COULD BE FUCKY.
impl<T> std::clone::Clone for Ref<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}
impl<T> std::marker::Copy for Ref<T> {}

impl<T> Ref<T> {
    pub fn value(&self) -> &T {
        unsafe { &self.inner.as_ref().value }
    }

    pub fn value_mut(&mut self) -> &mut T {
        unsafe { &mut self.inner.as_mut().value }
    }

    pub fn ty(&self) -> ObjType {
        self.header().ty
    }

    pub fn with_type_erased(self) -> Ref<()> {
        unsafe { mem::transmute(self) }
    }

    fn header(&self) -> &GcHeader {
        unsafe { &self.inner.as_ref().header }
    }

    fn header_mut(&mut self) -> &mut GcHeader {
        // MOST DEFINITELY NOT THREAD SAFE
        unsafe { &mut self.inner.as_mut().header }
    }

    // TODO: OBLITERATE? I DON'T KNOW IF I WANT THIS BEHAVIOR ANYWHERE
    pub fn has_same_ptr_as<X>(&self, other: &Ref<X>) -> bool {
        self.inner.as_ptr() as usize == other.inner.as_ptr() as usize
    }

    unsafe fn from_header(header: *mut GcHeader) -> Ref<T> {
        Ref {
            inner: mem::transmute(header),
        }
    }

    pub const fn dangling() -> Ref<T> {
        Ref {
            inner: NonNull::dangling(),
        }
    }
}

impl Ref<()> {
    pub unsafe fn force_into<T>(self) -> Ref<T> {
        mem::transmute(self)
    }
}

#[repr(C)]
struct RefStorage<T> {
    header: GcHeader,
    value: T,
}

#[repr(C)]
struct GcHeader {
    ty: ObjType,
    is_marked: bool,
    next: *mut GcHeader,
}

pub fn allocate_object<T>(ty: ObjType) -> Ref<T> {
    let size = mem::size_of::<RefStorage<T>>();
    let align = mem::align_of::<RefStorage<T>>();

    let object = reallocate(ptr::null_mut(), 0, size, align) as *mut RefStorage<T>;
    assert!(
        object != ptr::null_mut(),
        "Hopefully unnecesary sanity check."
    );

    unsafe {
        let header = &mut (*object).header;
        header.ty = ty;
        header.is_marked = false;

        let current_head = GC.objects;
        GC.objects = header;
        header.next = current_head;

        #[cfg(feature = "debug_log_gc")]
        {
            println!("{:?} allocate {} for {:?}", object, size, ty);
        }

        Ref {
            inner: mem::transmute(object),
        }
    }
}

const GC_HEAP_GROW_FACTOR: usize = 2;

// http://gchandbook.org/
pub struct GarbageCollector {
    vm: *mut VM,
    parser: *mut Parser<'static>,
    bytes_allocated: usize,
    next_gc: usize,
    strings: Table,
    objects: *mut GcHeader,
    gray_stack: Vec<*mut GcHeader>,
}
pub static mut GC: GarbageCollector = GarbageCollector {
    vm: ptr::null_mut(),
    parser: ptr::null_mut(),
    bytes_allocated: 0,
    next_gc: 1024 * 1024,
    strings: Table::new(),
    objects: ptr::null_mut(),
    gray_stack: vec![],
};

pub fn reallocate(
    pointer: *mut c_void,
    old_size: usize,
    new_size: usize,
    align: usize,
) -> *mut c_void {
    #[cfg(feature = "debug_log_gc")]
    {
        println!(
            "++++ reallocate(pointer = {:?}, old_size = {}, new_size = {}, align = {})",
            pointer, old_size, new_size, align
        );
    }

    let gc = unsafe { &mut GC };

    gc.bytes_allocated -= old_size;
    gc.bytes_allocated += new_size;

    if new_size > old_size {
        if cfg!(feature = "debug_stress_gc") || gc.bytes_allocated > gc.next_gc {
            unsafe { collect_garbage() };
        }
    }

    let result = if pointer == ptr::null_mut() {
        let layout = alloc::Layout::from_size_align(new_size, align).unwrap();
        let result = unsafe { alloc::alloc(layout) };
        assert!(
            result != ptr::null_mut(),
            "reallocate(pointer = {:?}, old_size = {}, new_size = {}, align = {}) -> {:?}",
            pointer,
            old_size,
            new_size,
            align,
            result
        );
        result
    } else {
        let layout = alloc::Layout::from_size_align(old_size, align).unwrap();
        if new_size == 0 {
            unsafe {
                alloc::dealloc(pointer as *mut u8, layout);
            }
            ptr::null_mut()
        } else {
            let result = unsafe { alloc::realloc(pointer as *mut u8, layout, new_size) };
            assert!(
                result != ptr::null_mut(),
                "reallocate(pointer = {:?}, old_size = {}, new_size = {}, align = {}) -> {:?}",
                pointer,
                old_size,
                new_size,
                align,
                result
            );

            result
        }
    };
    #[cfg(feature = "debug_log_gc")]
    {
        println!("---- reallocate -> {:?}", result);
    }
    result as *mut c_void
}

pub unsafe fn gc_track_vm(vm: *mut VM) {
    GC.vm = vm;
}

pub unsafe fn gc_track_constant_for_chunk_or_strings_table(value: Value) {
    let vm = &mut *GC.vm;
    vm.push(value);
}

pub unsafe fn gc_untrack_constant_for_chunk_or_strings_table() {
    let vm = &mut *GC.vm;
    vm.pop();
}

pub unsafe fn gc_intern_string(string: Ref<ObjString>) {
    GC.strings.set(string, Value::Nil);
}

pub unsafe fn gc_find_interned(chars: &[u8], hash: u32) -> Option<Ref<ObjString>> {
    GC.strings.find_string(chars, hash)
}

pub unsafe fn gc_track_parser(parser: *mut Parser<'static>) {
    GC.parser = parser;
}

pub unsafe fn gc_untrack_parser(_parser: *mut Parser<'static>) {
    GC.parser = ptr::null_mut();
}

// TODO: ACTUALLY THIS FUNCTION IS DEFINITELY 'unsafe' BUT I
// CAN'T BE BOTHERED TO CHANGE THAT IN THE MIDDLE OF A BIG REFACTOR
fn mark_header(header: *mut GcHeader) {
    if unsafe { (*header).is_marked } {
        return;
    }

    #[cfg(feature = "debug_log_gc")]
    {
        print!("{:?} mark ", object);
        unsafe {
            print_value(obj_val(object));
        }
        println!();
    }

    unsafe {
        (*header).is_marked = true;
        GC.gray_stack.push(header);
    }
}

pub fn mark_object<T>(mut object: Ref<T>) {
    mark_header(object.header_mut());
}

pub fn mark_value(value: Value) {
    if let Some(r) = value.as_erased_ref() {
        mark_object(r);
    }
}

fn mark_array(array: *mut Array<Value>) {
    for i in 0..unsafe { (*array).count() } {
        mark_value(unsafe { (*array)[i] });
    }
}

unsafe fn blacken_object(header: *mut GcHeader) {
    #[cfg(feature = "debug_log_gc")]
    {
        print!("{:?} blacken ", object);
        print_value(obj_val(object));
        println!();
    }

    match (*header).ty {
        ObjType::List => {
            let list: Ref<ObjList> = Ref::from_header(header);
            for val in &list.value().items {
                mark_value(*val);
            }
        }
        ObjType::Closure => {
            let mut closure: Ref<ObjClosure> = Ref::from_header(header);
            mark_object(closure.value_mut().function);
            for i in 0..closure.value().upvalues.count() {
                if let Some(uv) = closure.value().upvalues[i] {
                    mark_object(uv);
                }
            }
        }
        ObjType::Function => {
            let mut function: Ref<ObjFunction> = Ref::from_header(header);
            if let Some(name) = function.value().name {
                mark_object(name);
            }
            mark_array(&mut function.value_mut().chunk.constants);
        }
        ObjType::Upvalue => {
            let upvalue: Ref<ObjUpvalue> = Ref::from_header(header);
            mark_value(upvalue.value().closed)
        }
        ObjType::Class => {
            let mut class: Ref<ObjClass> = Ref::from_header(header);
            mark_object(class.value_mut().name);
            class.value_mut().methods.mark_table();
        }
        ObjType::Instance => {
            let mut instance: Ref<ObjInstance> = Ref::from_header(header);
            mark_object(instance.value_mut().class);
            instance.value_mut().fields.mark_table();
        }
        ObjType::BoundMethod => {
            let mut bound: Ref<ObjBoundMethod> = Ref::from_header(header);
            mark_value(bound.value().receiver);
            mark_object(bound.value_mut().method);
        }
        ObjType::Native | ObjType::String => {}
    }
}

unsafe fn free_object(header: *mut GcHeader) {
    #[cfg(feature = "debug_log_gc")]
    {
        println!("{:?} free type {:?}", object, (*object).ty);
    }

    match (*header).ty {
        ObjType::List => {
            let mut list: Ref<ObjList> = Ref::from_header(header);
            list.value_mut().items.clear();
            list.value_mut().items.shrink_to_fit();
            free(list);
        }
        ObjType::Function => {
            let mut function: Ref<ObjFunction> = Ref::from_header(header);
            free_chunk(&mut function.value_mut().chunk);
            free(function);
        }
        ObjType::Native => {
            let native: Ref<ObjNative> = Ref::from_header(header);
            free(native);
        }
        ObjType::String => {
            let mut string: Ref<ObjString> = Ref::from_header(header);
            string.value_mut().chars.free();
            free(string);
        }
        ObjType::Upvalue => {
            let upvalue: Ref<ObjUpvalue> = Ref::from_header(header);
            free(upvalue);
        }
        ObjType::Closure => {
            let mut closure: Ref<ObjClosure> = Ref::from_header(header);
            closure.value_mut().upvalues.free();
            free(closure);
        }
        ObjType::Class => {
            let mut class: Ref<ObjClass> = Ref::from_header(header);
            class.value_mut().methods.free();
            free(class);
        }
        ObjType::Instance => {
            let mut instance: Ref<ObjInstance> = Ref::from_header(header);
            instance.value_mut().fields.free();
            free(instance);
        }
        ObjType::BoundMethod => {
            let bound: Ref<ObjBoundMethod> = Ref::from_header(header);
            free(bound);
        }
    }
}

unsafe fn mark_roots() {
    let vm = &mut *GC.vm;
    let mut slot = vm.stack.as_mut_ptr();
    while slot < vm.stack_top {
        mark_value(*slot);
        slot = slot.add(1);
    }

    for i in 0..vm.frame_count {
        mark_object(vm.frames[i as usize].closure);
    }

    let mut upvalue = vm.open_upvalues;
    while let Some(uv) = upvalue {
        mark_object(uv);
        upvalue = uv.value().next;
    }

    vm.globals.mark_table();
    if GC.parser != ptr::null_mut() {
        (*GC.parser).mark_compiler_roots();
    }
    mark_object(vm.init_string);
}

unsafe fn trace_references() {
    while let Some(object) = GC.gray_stack.pop() {
        blacken_object(object);
    }
}

unsafe fn sweep() {
    let mut previous = ptr::null_mut();
    let mut object = GC.objects;
    while object != ptr::null_mut() {
        if (*object).is_marked {
            (*object).is_marked = false;
            previous = object;
            object = (*object).next;
        } else {
            let unreached = object;
            object = (*object).next;
            if previous != ptr::null_mut() {
                (*previous).next = object;
            } else {
                GC.objects = object;
            }

            free_object(unreached);
        }
    }
}

unsafe fn remove_white(table: &mut Table) {
    let mut delet = vec![];
    for entry in &table[..] {
        match entry.key() {
            Some(key) if !key.header().is_marked => {
                delet.push(key);
            }
            _ => {}
        }
    }
    for d in delet {
        table.delete(d);
    }
}

unsafe fn collect_garbage() {
    #[cfg(feature = "debug_log_gc")]
    {
        println!("-- gc begin");
    }
    #[allow(unused_variables)]
    let before = GC.bytes_allocated;

    mark_roots();
    trace_references();
    remove_white(&mut GC.strings);
    sweep();

    GC.next_gc = GC.bytes_allocated * GC_HEAP_GROW_FACTOR;

    #[cfg(feature = "debug_log_gc")]
    {
        println!("-- gc end");
        println!(
            "   collected {} bytes (from {} to {}) next at {}",
            before - GC.bytes_allocated,
            before,
            GC.bytes_allocated,
            GC.next_gc
        );
    }
}

pub unsafe fn free_objects() {
    let mut object = GC.objects;
    while object != ptr::null_mut() {
        let next = (*object).next;
        free_object(object);
        object = next;
    }
}
