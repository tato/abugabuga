use std::{ops::{Index, IndexMut, Range}, ptr, slice};


use crate::{
    memory::{mark_object, mark_value},
    object::{Obj, ObjString},
    value::{bool_val, is_nil, Value, NIL_VAL},
};


fn grow_capacity(capacity: usize) -> usize {
    if capacity < 8 {
        8
    } else {
        capacity * 2
    }
}

macro_rules! grow_array {
    ($t:ty, $pointer:expr, $old_count:expr, $new_count:expr) => {
        crate::memory::reallocate(
            $pointer as *mut std::ffi::c_void,
            std::mem::size_of::<$t>() * $old_count as usize,
            std::mem::size_of::<$t>() * $new_count as usize,
            std::mem::align_of::<$t>(),
        ) as *mut $t
    };
}

macro_rules! free_array {
    ($t:ty, $pointer:expr, $old_count:expr) => {
        crate::memory::reallocate(
            $pointer as *mut std::ffi::c_void,
            std::mem::size_of::<$t>() * $old_count as usize,
            0,
            std::mem::align_of::<$t>(),
        )
    };
}

pub struct Array<T> {
    capacity: usize,
    count: usize,
    values: *mut T,
}

impl<T> Array<T> {
    pub fn new() -> Self {
        Array {
            capacity: 0,
            count: 0,
            values: ptr::null_mut(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Array {
            capacity,
            count: 0,
            values: grow_array!(T, ptr::null_mut(), 0, capacity),
        }
    }

    pub fn write(&mut self, value: T) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(self.capacity);
            self.values = grow_array!(T, self.values, old_capacity, self.capacity);
        }
        unsafe {
            *self.values.add(self.count) = value;
        }
        self.count += 1;
    }

    // TODO: search "as i32" for conversions from this count to int
    pub fn count(&self) -> usize {
        self.count
    }

    pub fn free(&mut self) {
        free_array!(T, self.values, self.capacity);
    }
}

impl<T> Index<usize> for Array<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.count, "Index out of range! (Index = {}, Count = {})", index, self.count);
        unsafe {
            &*self.values.add(index)
        }
    }
}

impl<T> IndexMut<usize> for Array<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.count, "Index out of range! (Index = {}, Count = {})", index, self.count);
        unsafe {
            &mut *self.values.add(index)
        }
    }
}

impl<T> Index<Range<usize>> for Array<T> {
    type Output = [T];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        if index.start >= index.end {
            return &[];
        }
        assert!(
            index.end <= self.count, 
            "Invalid range! end too big (Range = {:?}, Count = {})", index, self.count
        );
        unsafe {
            slice::from_raw_parts(self.values.add(index.start), index.end - index.start)
        }
    }
}

impl<T> From<&[T]> for Array<T> {
    fn from(f: &[T]) -> Self {
        let mut array = Array::with_capacity(f.len());
        unsafe {
            ptr::copy_nonoverlapping(f.as_ptr(), array.values, f.len());
        }
        array.count = f.len();
        array
    }
}










const TABLE_MAX_LOAD: f32 = 0.75;

#[derive(Debug)]
pub struct Table {
    pub count: usize,
    pub capacity: usize,
    pub entries: *mut Entry,
}

pub struct Entry {
    key: *mut ObjString,
    value: Value,
}

pub unsafe fn init_table(table: *mut Table) {
    let table = &mut *table;
    table.count = 0;
    table.capacity = 0;
    table.entries = ptr::null_mut();
}

pub unsafe fn free_table(table: *mut Table) {
    let table = &mut *table;
    free_array!(Entry, table.entries, table.capacity);
    init_table(table);
}

unsafe fn find_entry(entries: *mut Entry, capacity: usize, key: *mut ObjString) -> *mut Entry {
    let key = &mut *key;
    let mut index = key.hash & ((capacity - 1) as u32);
    let mut tombstone = ptr::null_mut();

    loop {
        let entry = &mut *entries.offset(index as isize);
        if entry.key == ptr::null_mut() {
            if is_nil(entry.value) {
                // empty entry
                return if tombstone != ptr::null_mut() {
                    tombstone
                } else {
                    entry
                };
            } else {
                // we found a tombstone
                if tombstone == ptr::null_mut() {
                    tombstone = entry;
                }
            }
        } else if entry.key == key {
            return entry;
        }

        index = (index + 1) & ((capacity - 1) as u32);
    }
}

unsafe fn adjust_capacity(table: *mut Table, capacity: usize) {
    let entries = allocate!(Entry, capacity);
    for i in 0..capacity {
        let e = &mut *entries.offset(i as isize);
        e.key = ptr::null_mut();
        e.value = NIL_VAL;
    }

    let table = &mut *table;
    table.count = 0;

    for i in 0..table.capacity {
        let entry = &mut *table.entries.offset(i as isize);
        if entry.key == ptr::null_mut() {
            continue;
        }

        let dest = &mut *find_entry(entries, capacity, entry.key);
        dest.key = entry.key;
        dest.value = entry.value;
        table.count += 1;
    }

    free_array!(Entry, table.entries, table.capacity);
    table.entries = entries;
    table.capacity = capacity;
}

pub unsafe fn table_get(table: *mut Table, key: *mut ObjString, value: *mut Value) -> bool {
    let table = &mut *table;

    if table.count == 0 {
        return false;
    }

    let entry = &mut *find_entry(table.entries, table.capacity, key);
    if entry.key == ptr::null_mut() {
        return false;
    }

    *value = entry.value;
    true
}

pub unsafe fn table_set(table: *mut Table, key: *mut ObjString, value: Value) -> bool {
    let table = &mut *table;

    if table.count as f32 + 1.0 > table.capacity as f32 * TABLE_MAX_LOAD {
        let capacity = grow_capacity(table.capacity);
        adjust_capacity(table, capacity);
    }

    let entry = &mut *find_entry(table.entries, table.capacity, key);
    let is_new_key = entry.key == ptr::null_mut();
    if is_new_key && is_nil(entry.value) {
        table.count += 1;
    }

    entry.key = key;
    entry.value = value;
    is_new_key
}

pub unsafe fn table_delete(table: *mut Table, key: *mut ObjString) -> bool {
    let table = &mut *table;

    if table.count == 0 {
        return false;
    }

    let entry = &mut *find_entry(table.entries, table.capacity, key);
    if entry.key == ptr::null_mut() {
        return false;
    }

    entry.key = ptr::null_mut();
    entry.value = bool_val(true);

    true
}

pub unsafe fn table_add_all(from: *const Table, to: *mut Table) {
    let from = &*from;
    for i in 0..from.capacity {
        let entry = &*from.entries.offset(i as isize);
        if entry.key != ptr::null_mut() {
            table_set(to, entry.key, entry.value);
        }
    }
}

pub unsafe fn table_find_string(table: *mut Table, chars: &[u8], hash: u32) -> *mut ObjString {
    let table = &mut *table;
    if table.count == 0 {
        return ptr::null_mut();
    }

    let mut index = hash & ((table.capacity - 1) as u32);
    loop {
        let entry = &mut *table.entries.offset(index as isize);
        if entry.key == ptr::null_mut() {
            if is_nil(entry.value) {
                return ptr::null_mut();
            }
        } else {
            let key = &mut *entry.key;
            if key.chars.count() == chars.len()
                && key.hash == hash
                && &key.chars[0..key.chars.count()] == chars
            {
                return key;
            }
        }

        index = (index + 1) & ((table.capacity - 1) as u32);
    }
}

pub unsafe fn table_remove_white(table: *mut Table) {
    for i in 0..(*table).capacity {
        let entry = (*table).entries.offset(i as isize);
        if (*entry).key != ptr::null_mut() && !(*(*entry).key).obj.is_marked {
            table_delete(table, (*entry).key);
        }
    }
}

pub unsafe fn mark_table(table: *mut Table) {
    for i in 0..(*table).capacity {
        let entry = (*table).entries.offset(i as isize);
        mark_object((*entry).key as *mut Obj);
        mark_value((*entry).value);
    }
}
