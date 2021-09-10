use std::{alloc, ffi::c_void, ptr, vec};

use crate::{
    array::{Array, Table},
    chunk::free_chunk,
    compiler::Parser,
    object::{
        Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjList, ObjNative,
        ObjString, ObjType, ObjUpvalue,
    },
    value::{as_obj, is_obj, Value, NIL_VAL},
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

fn free<T>(pointer: *mut T) {
    reallocate(
        pointer as *mut std::ffi::c_void,
        std::mem::size_of::<T>(),
        0,
        std::mem::align_of::<T>(),
    );
}

const GC_HEAP_GROW_FACTOR: usize = 2;

// http://gchandbook.org/
pub struct GarbageCollector {
    pub vm: *mut VM,
    parser: *mut Parser<'static>,
    bytes_allocated: usize,
    next_gc: usize,
    strings: Table,
    objects: *mut Obj<()>,
    gray_stack: Vec<*mut Obj<()>>,
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

pub unsafe fn gc_track_constant_for_chunk_or_strings_table(value: Value) {
    let vm = &mut *GC.vm;
    vm.push(value);
}

pub unsafe fn gc_untrack_constant_for_chunk_or_strings_table() {
    let vm = &mut *GC.vm;
    vm.pop();
}

pub unsafe fn gc_track_object(object: *mut Obj<()>) -> *mut Obj<()> {
    let result = GC.objects;
    GC.objects = object;
    result
}

pub unsafe fn gc_intern_string(string: *mut Obj<ObjString>) {
    GC.strings.set(string, NIL_VAL);
}

pub unsafe fn gc_find_interned(chars: &[u8], hash: u32) -> *mut Obj<ObjString> {
    GC.strings.find_string(chars, hash)
}

pub unsafe fn gc_track_parser(parser: *mut Parser<'static>) {
    GC.parser = parser;
}

pub unsafe fn gc_untrack_parser(_parser: *mut Parser<'static>) {
    GC.parser = ptr::null_mut();
}

pub fn mark_object(object: *mut Obj<()>) {
    if object == ptr::null_mut() {
        return;
    }
    if unsafe { (*object).is_marked } {
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
        (*object).is_marked = true;
    }

    unsafe {
        GC.gray_stack.push(object);
    }
}

pub fn mark_value(value: Value) {
    if is_obj(value) {
        mark_object(as_obj(value));
    }
}

fn mark_array(array: *mut Array<Value>) {
    for i in 0..unsafe { (*array).count() } {
        mark_value(unsafe { (*array)[i] });
    }
}

unsafe fn blacken_object(object: *mut Obj<()>) {
    #[cfg(feature = "debug_log_gc")]
    {
        print!("{:?} blacken ", object);
        print_value(obj_val(object));
        println!();
    }

    match (*object).ty {
        ObjType::List => {
            let list = object as *mut Obj<ObjList>;
            for val in &(*list).value.items {
                mark_value(*val);
            }
        }
        ObjType::Closure => {
            let closure = object as *mut Obj<ObjClosure>;
            mark_object((*closure).value.function as *mut Obj<()>);
            for i in 0..(*closure).value.upvalues.count() {
                mark_object((*closure).value.upvalues[i] as *mut Obj<()>);
            }
        }
        ObjType::Function => {
            let function = object as *mut Obj<ObjFunction>;
            mark_object((*function).value.name as *mut Obj<()>);
            mark_array(&mut (*function).value.chunk.constants);
        }
        ObjType::Upvalue => mark_value((*(object as *mut Obj<ObjUpvalue>)).value.closed),
        ObjType::Class => {
            let class = object as *mut Obj<ObjClass>;
            mark_object((*class).value.name as *mut Obj<()>);
            (*class).value.methods.mark_table();
        }
        ObjType::Instance => {
            let instance = object as *mut Obj<ObjInstance>;
            mark_object((*instance).value.class as *mut Obj<()>);
            (*instance).value.fields.mark_table();
        }
        ObjType::BoundMethod => {
            let bound = object as *mut Obj<ObjBoundMethod>;
            mark_value((*bound).value.receiver);
            mark_object((*bound).value.method as *mut Obj<()>);
        }
        ObjType::Native | ObjType::String => {}
    }
}

unsafe fn free_object(object: *mut Obj<()>) {
    #[cfg(feature = "debug_log_gc")]
    {
        println!("{:?} free type {:?}", object, (*object).ty);
    }

    match (*object).ty {
        ObjType::List => {
            let list = object as *mut Obj<ObjList>;
            (*list).value.items.clear();
            (*list).value.items.shrink_to_fit();
            free(list);
        }
        ObjType::Function => {
            let function = object as *mut Obj<ObjFunction>;
            free_chunk(&mut (*function).value.chunk);
            free(function);
        }
        ObjType::Native => {
            free(object as *mut Obj<ObjNative>);
        }
        ObjType::String => {
            let string = object as *mut Obj<ObjString>;
            (*string).value.chars.free();
            free(string);
        }
        ObjType::Upvalue => {
            free(object as *mut Obj<ObjUpvalue>);
        }
        ObjType::Closure => {
            let closure = object as *mut Obj<ObjClosure>;
            (*closure).value.upvalues.free();
            free(closure);
        }
        ObjType::Class => {
            let class = object as *mut Obj<ObjClass>;
            (*class).value.methods.free();
            free(class);
        }
        ObjType::Instance => {
            let instance = object as *mut Obj<ObjInstance>;
            (*instance).value.fields.free();
            free(instance);
        }
        ObjType::BoundMethod => {
            free(object as *mut Obj<ObjBoundMethod>);
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
        mark_object(vm.frames[i as usize].closure as *mut Obj<()>);
    }

    let mut upvalue = vm.open_upvalues;
    while upvalue != ptr::null_mut() {
        mark_object(upvalue as *mut Obj<()>);
        upvalue = (*upvalue).value.next;
    }

    vm.globals.mark_table();
    if GC.parser != ptr::null_mut() {
        (*GC.parser).mark_compiler_roots();
    }
    mark_object(vm.init_string as *mut Obj<()>);
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

unsafe fn collect_garbage() {
    #[cfg(feature = "debug_log_gc")]
    {
        println!("-- gc begin");
    }
    #[allow(unused_variables)]
    let before = GC.bytes_allocated;

    mark_roots();
    trace_references();
    GC.strings.remove_white();
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
