use std::{ffi::c_void, ptr, vec};

use crate::{chunk::free_chunk, compiler::Parser, object::{Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjList, ObjNative, ObjString, ObjType, ObjUpvalue}, table::{Table, free_table, mark_table, table_find_string, table_remove_white, table_set}, value::{NIL_VAL, Value, ValueArray, as_obj, is_obj}, vm::VM};

#[cfg(feature = "debug_log_gc")]
use crate::value::{obj_val, print_value};

macro_rules! allocate {
    ($t:ty, $count:expr) => {
        crate::memory::reallocate(
            std::ptr::null_mut(),
            0,
            std::mem::size_of::<$t>() * $count as usize,
        ) as *mut $t
    };
}

macro_rules! free {
    ($t:ty, $pointer:expr) => {
        crate::memory::reallocate(
            $pointer as *mut std::ffi::c_void,
            std::mem::size_of::<$t>(),
            0,
        )
    };
}

macro_rules! grow_capacity {
    ($capacity:expr) => {
        if $capacity < 8 {
            8
        } else {
            $capacity * 2
        }
    };
}

macro_rules! grow_array {
    ($t:ty, $pointer:expr, $old_count:expr, $new_count:expr) => {
        crate::memory::reallocate(
            $pointer as *mut std::ffi::c_void,
            std::mem::size_of::<$t>() * $old_count as usize,
            std::mem::size_of::<$t>() * $new_count as usize,
        ) as *mut $t
    };
}

macro_rules! free_array {
    ($t:ty, $pointer:expr, $old_count:expr) => {
        crate::memory::reallocate(
            $pointer as *mut std::ffi::c_void,
            std::mem::size_of::<$t>() * $old_count as usize,
            0,
        )
    };
}

const GC_HEAP_GROW_FACTOR: usize = 2;

pub struct GarbageCollector {
    pub vm: *mut VM,
    bytes_allocated: usize,
    next_gc: usize,
    strings: Table,
    objects: *mut Obj,
    gray_stack: Vec<*mut Obj>,
}
pub static mut GC: GarbageCollector = GarbageCollector{ 
    vm: ptr::null_mut(),
    bytes_allocated: 0,
    next_gc: 1024 * 1024,
    strings: Table {
        capacity: 0,
        count: 0,
        entries: ptr::null_mut(),
    },
    objects: ptr::null_mut(),
    gray_stack: vec![],
};

pub unsafe fn reallocate(pointer: *mut c_void, old_size: usize, new_size: usize) -> *mut c_void {
    if new_size > old_size {
        GC.bytes_allocated += new_size - old_size;
    } else {
        GC.bytes_allocated -= old_size - new_size;
    }

    if new_size > old_size {
        #[cfg(feature = "debug_stress_gc")]
        {
            collect_garbage();
        }

        if GC.bytes_allocated > GC.next_gc {
            collect_garbage();
        }
    }
    if new_size == 0 {
        let _free = Vec::<u8>::from_raw_parts(pointer as *mut u8, old_size, old_size);
        return ptr::null_mut();
    }

    let result = vec![0u8; new_size].leak().as_mut_ptr();
    if !pointer.is_null() {
        ptr::copy(pointer as *mut u8, result, old_size);
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

pub unsafe fn gc_track_object(object: *mut Obj) -> *mut Obj {
    let result = GC.objects;
    GC.objects = object;
    result
}

pub unsafe fn gc_intern_string(string: *mut ObjString) {
    table_set(&mut GC.strings, string, NIL_VAL);
}

pub unsafe fn gc_find_interned(
    chars: *const u8,
    length: i32,
    hash: u32) -> *mut ObjString {
    table_find_string(&mut GC.strings, chars, length, hash)
}

pub unsafe fn mark_object(object: *mut Obj) {
    if object == ptr::null_mut() {
        return;
    }
    if (*object).is_marked {
        return;
    }

    #[cfg(feature = "debug_log_gc")]
    {
        print!("{:?} mark ", object);
        print_value(obj_val(object));
        println!();
    }

    (*object).is_marked = true;

    GC.gray_stack.push(object);
}

pub unsafe fn mark_value(value: Value) {
    if is_obj(value) {
        mark_object(as_obj(value));
    }
}

unsafe fn mark_array(array: *mut ValueArray) {
    for i in 0..(*array).count {
        mark_value(*(*array).values.offset(i as isize));
    }
}

unsafe fn blacken_object(object: *mut Obj) {
    #[cfg(feature = "debug_log_gc")]
    {
        print!("{:?} blacken ", object);
        print_value(obj_val(object));
        println!();
    }

    match (*object).ty {
        ObjType::List => {
            let list = object as *mut ObjList;
            for val in &(*list).items {
                mark_value(*val);
            }
        }
        ObjType::Closure => {
            let closure = object as *mut ObjClosure;
            mark_object((*closure).function as *mut Obj);
            for i in 0..(*closure).upvalue_count {
                mark_object((*closure).upvalues.offset(i as isize) as *mut Obj);
            }
        }
        ObjType::Function => {
            let function = object as *mut ObjFunction;
            mark_object((*function).name as *mut Obj);
            mark_array(&mut (*function).chunk.constants);
        }
        ObjType::Upvalue => mark_value((*(object as *mut ObjUpvalue)).closed),
        ObjType::Class => {
            let class = object as *mut ObjClass;
            mark_object((*class).name as *mut Obj);
            mark_table(&mut (*class).methods);
        }
        ObjType::Instance => {
            let instance = object as *mut ObjInstance;
            mark_object((*instance).class as *mut Obj);
            mark_table(&mut (*instance).fields);
        }
        ObjType::BoundMethod => {
            let bound = object as *mut ObjBoundMethod;
            mark_value((*bound).receiver);
            mark_object((*bound).method as *mut Obj);
        }
        ObjType::Native | ObjType::String => {}
    }
}

unsafe fn free_object(object: *mut Obj) {
    #[cfg(feature = "debug_log_gc")]
    {
        println!("{:?} free type {:?}", object, (*object).ty);
    }

    match (*object).ty {
        ObjType::List => {
            let list = object as *mut ObjList;
            (*list).items.clear();
            (*list).items.shrink_to_fit();
            free!(ObjList, object);
        }
        ObjType::Function => {
            let function = object as *mut ObjFunction;
            free_chunk(&mut (*function).chunk);
            free!(ObjFunction, object);
        }
        ObjType::Native => {
            free!(ObjNative, object);
        }
        ObjType::String => {
            let string = object as *mut ObjString;
            free_array!(u8, (*string).chars, (*string).length + 1);
            free!(ObjString, object);
        }
        ObjType::Upvalue => {
            free!(ObjUpvalue, object);
        }
        ObjType::Closure => {
            let closure = object as *mut ObjClosure;
            free_array!(
                
                *mut ObjUpvalue,
                (*closure).upvalues,
                (*closure).upvalue_count
            );
            free!(ObjClosure, object);
        }
        ObjType::Class => {
            let class = object as *mut ObjClass;
            free_table(&mut (*class).methods);
            free!(ObjClass, object);
        }
        ObjType::Instance => {
            let instance = object as *mut ObjInstance;
            free_table(&mut (*instance).fields);
            free!(ObjInstance, object);
        }
        ObjType::BoundMethod => {
            free!(ObjBoundMethod, object);
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
        mark_object(vm.frames[i as usize].closure as *mut Obj);
    }

    let mut upvalue = vm.open_upvalues;
    while upvalue != ptr::null_mut() {
        mark_object(upvalue as *mut Obj);
        upvalue = (*upvalue).next;
    }

    mark_table(&mut vm.globals);
    let parser: *mut Parser = vm.parser.as_mut().unwrap();
    (*parser).mark_compiler_roots();
    mark_object(vm.init_string as *mut Obj);
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
    table_remove_white(&mut GC.strings);
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
