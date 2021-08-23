use std::{ffi::c_void, ptr};

use crate::{
    chunk::free_chunk,
    compiler::mark_compiler_roots,
    object::{
        Obj, ObjBoundMethod, ObjClass, ObjClosure, ObjFunction, ObjInstance, ObjNative, ObjString,
        ObjType, ObjUpvalue,
    },
    table::{free_table, mark_table, table_remove_white},
    value::{as_obj, is_obj, Value, ValueArray},
    vm::vm,
};

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

pub unsafe fn reallocate(pointer: *mut c_void, old_size: usize, new_size: usize) -> *mut c_void {
    if new_size > old_size {
        vm.bytes_allocated += new_size - old_size;
    } else {
        vm.bytes_allocated -= old_size - new_size;
    }

    if new_size > old_size {
        #[cfg(feature = "debug_stress_gc")]
        {
            collect_garbage();
        }

        if vm.bytes_allocated > vm.next_gc {
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

    vm.gray_stack.push(object);
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
    mark_compiler_roots();
    mark_object(vm.init_string as *mut Obj);
}

unsafe fn trace_references() {
    while let Some(object) = vm.gray_stack.pop() {
        blacken_object(object);
    }
}

unsafe fn sweep() {
    let mut previous = ptr::null_mut();
    let mut object = vm.objects;
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
                vm.objects = object;
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
    let before = vm.bytes_allocated;

    mark_roots();
    trace_references();
    table_remove_white(&mut vm.strings);
    sweep();

    vm.next_gc = vm.bytes_allocated * GC_HEAP_GROW_FACTOR;

    #[cfg(feature = "debug_log_gc")]
    {
        println!("-- gc end");
        println!(
            "   collected {} bytes (from {} to {}) next at {}",
            before - vm.bytes_allocated,
            before,
            vm.bytes_allocated,
            vm.next_gc
        );
    }
}

pub unsafe fn free_objects() {
    let mut object = vm.objects;
    while object != ptr::null_mut() {
        let next = (*object).next;
        free_object(object);
        object = next;
    }
}
