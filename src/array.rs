use std::{
    ops::{Index, IndexMut, Range, RangeFull},
    ptr, slice,
};

use crate::{
    memory::{self, mark_object, mark_value},
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

fn grow_array<T>(pointer: *mut T, old_count: usize, new_count: usize) -> *mut T {
    memory::reallocate(
        pointer as *mut std::ffi::c_void,
        std::mem::size_of::<T>() * old_count,
        std::mem::size_of::<T>() * new_count,
        std::mem::align_of::<T>(),
    ) as *mut T
}

fn free_array<T>(pointer: *mut T, old_count: usize) {
    memory::reallocate(
        pointer as *mut std::ffi::c_void,
        std::mem::size_of::<T>() * old_count,
        0,
        std::mem::align_of::<T>(),
    );
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
            values: grow_array(ptr::null_mut(), 0, capacity),
        }
    }

    pub fn write(&mut self, value: T) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.capacity = grow_capacity(self.capacity);
            self.values = grow_array(self.values, old_capacity, self.capacity);
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

    // TODO: impl Drop
    pub fn free(&mut self) {
        free_array(self.values, self.capacity);
    }
}

impl<T> Index<usize> for Array<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        assert!(
            index < self.count,
            "Index out of range! (Index = {}, Count = {})",
            index,
            self.count
        );
        unsafe { &*self.values.add(index) }
    }
}

impl<T> IndexMut<usize> for Array<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(
            index < self.count,
            "Index out of range! (Index = {}, Count = {})",
            index,
            self.count
        );
        unsafe { &mut *self.values.add(index) }
    }
}

impl<T> Index<Range<usize>> for Array<T> {
    type Output = [T];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        if index.end <= index.start {
            return &[];
        }
        assert!(
            index.end <= self.count,
            "Invalid range! end too big (Range = {:?}, Count = {})",
            index,
            self.count
        );
        unsafe { slice::from_raw_parts(self.values.add(index.start), index.end - index.start) }
    }
}

impl<T> Index<RangeFull> for Array<T> {
    type Output = [T];

    fn index(&self, _index: RangeFull) -> &Self::Output {
        &self[0..self.count]
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

pub struct Table {
    count: usize,
    capacity: usize,
    entries: *mut Entry,
}

pub struct Entry {
    key: *mut Obj<ObjString>,
    value: Value,
}

impl Table {
    pub const fn new() -> Table {
        Table {
            capacity: 0,
            count: 0,
            entries: ptr::null_mut(),
        }
    }

    pub fn get(&mut self, key: *mut Obj<ObjString>) -> Option<Value> {
        if self.count == 0 {
            return None;
        }

        let entry = unsafe { &mut *find_entry(self.entries, self.capacity, key) };
        if entry.key == ptr::null_mut() {
            return None;
        }

        Some(entry.value)
    }

    pub fn set(&mut self, key: *mut Obj<ObjString>, value: Value) -> bool {
        if self.count as f32 + 1.0 > self.capacity as f32 * TABLE_MAX_LOAD {
            let capacity = grow_capacity(self.capacity);
            unsafe { adjust_capacity(self, capacity) };
        }

        let entry = unsafe { &mut *find_entry(self.entries, self.capacity, key) };
        let is_new_key = entry.key == ptr::null_mut();
        if is_new_key && is_nil(entry.value) {
            self.count += 1;
        }

        entry.key = key;
        entry.value = value;
        is_new_key
    }

    pub fn delete(&mut self, key: *mut Obj<ObjString>) -> bool {
        if self.count == 0 {
            return false;
        }

        let entry = unsafe { &mut *find_entry(self.entries, self.capacity, key) };
        if entry.key == ptr::null_mut() {
            return false;
        }

        entry.key = ptr::null_mut();
        entry.value = bool_val(true);

        true
    }

    pub fn add_all(&mut self, from: *const Table) {
        let from = unsafe { &*from };
        for i in 0..from.capacity {
            let entry = unsafe { &*from.entries.offset(i as isize) };
            if entry.key != ptr::null_mut() {
                self.set(entry.key, entry.value);
            }
        }
    }

    pub fn find_string(&mut self, chars: &[u8], hash: u32) -> *mut Obj<ObjString> {
        if self.count == 0 {
            return ptr::null_mut();
        }

        let mut index = hash & ((self.capacity - 1) as u32);
        loop {
            let entry = unsafe { &mut *self.entries.offset(index as isize) };
            if entry.key == ptr::null_mut() {
                if is_nil(entry.value) {
                    return ptr::null_mut();
                }
            } else {
                let key = unsafe { &mut *entry.key };
                if key.value.chars.count() == chars.len()
                    && key.value.hash == hash
                    && &key.value.chars[0..key.value.chars.count()] == chars
                {
                    return key;
                }
            }

            index = (index + 1) & ((self.capacity - 1) as u32);
        }
    }

    pub fn remove_white(&mut self) {
        for i in 0..self.capacity {
            unsafe {
                let entry = self.entries.offset(i as isize);
                if (*entry).key != ptr::null_mut() && !(*(*entry).key).is_marked {
                    self.delete((*entry).key);
                }
            }
        }
    }

    pub fn mark_table(&mut self) {
        for i in 0..self.capacity {
            unsafe {
                let entry = self.entries.offset(i as isize);
                mark_object((*entry).key as *mut Obj<()>);
                mark_value((*entry).value);
            }
        }
    }

    // TODO: impl Drop
    pub fn free(&mut self) {
        free_array(self.entries, self.capacity);
    }
}

fn find_entry(entries: *mut Entry, capacity: usize, key: *mut Obj<ObjString>) -> *mut Entry {
    let key = unsafe { &mut *key };

    let mut index = key.value.hash & ((capacity - 1) as u32);
    let mut tombstone = ptr::null_mut();

    loop {
        let entry = unsafe { &mut *entries.add(index as usize) };
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
    let entries: *mut Entry = memory::allocate(capacity);
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

    free_array(table.entries, table.capacity);
    table.entries = entries;
    table.capacity = capacity;
}
