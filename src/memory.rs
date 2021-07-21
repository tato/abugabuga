use std::{ffi::c_void, ptr};

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

pub unsafe fn reallocate(pointer: *mut c_void, old_size: usize, new_size: usize) -> *mut c_void {
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
