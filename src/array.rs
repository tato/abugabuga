use std::{
    ops::{Index, IndexMut, Range, RangeFull},
    ptr, slice,
};

use crate::{
    memory::{self, Ref},
    object::ObjString,
    value::Value,
};

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
        let values = unsafe { memory::reallocate(ptr::null_mut(), 0, capacity) };
        Array {
            capacity,
            count: 0,
            values,
        }
    }

    fn grow_capacity(&mut self) {
        if self.capacity < 8 {
            self.capacity = 8;
        } else {
            self.capacity *= 2;
        }
    }

    pub fn append(&mut self, value: T) {
        if self.capacity < self.count + 1 {
            let old_capacity = self.capacity;
            self.grow_capacity();
            self.values = unsafe { memory::reallocate(self.values, old_capacity, self.capacity) };
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
        unsafe {
            memory::reallocate(self.values, self.capacity, 0);
        }
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
    key: Option<Ref<ObjString>>,
    value: Value,
}

impl Entry {
    pub fn key(&self) -> Option<Ref<ObjString>> {
        self.key
    }
    pub fn value(&self) -> Value {
        self.value
    }
}

impl Table {
    pub const fn new() -> Table {
        Table {
            capacity: 0,
            count: 0,
            entries: ptr::null_mut(),
        }
    }

    pub fn get(&mut self, key: Ref<ObjString>) -> Option<Value> {
        if self.count == 0 {
            return None;
        }

        let entry = unsafe { &mut *find_entry(self.entries, self.capacity, key) };
        if entry.key.is_none() {
            return None;
        }

        Some(entry.value)
    }

    fn grow_capacity(&mut self) {
        if self.capacity < 8 {
            self.capacity = 8;
        } else {
            self.capacity *= 2;
        }
    }

    fn grow_pointer(&mut self) {
        let old_entries = self.entries;
        let old_capacity = self.capacity;

        self.grow_capacity();

        self.entries = unsafe {
            // SAFETY: ???
            memory::reallocate(ptr::null_mut(), 0, self.capacity)
        };
        for i in 0..self.capacity {
            // SAFETY: ???
            let e = unsafe { &mut *self.entries.offset(i as isize) };
            e.key = None;
            e.value = Value::NIL;
        }
    
        self.count = 0;
    
        for i in 0..old_capacity {
            // SAFETY: ???
            let entry = unsafe { &mut *old_entries.offset(i as isize) };
            match entry.key {
                None => continue,
                Some(key) => {
                    let dest = unsafe { &mut *find_entry(self.entries, self.capacity, key) };
                    dest.key = entry.key;
                    dest.value = entry.value;
                    self.count += 1;
                }
            }
        }
    
        unsafe {
            // SAFETY: ???
            memory::reallocate(old_entries, old_capacity, 0);
        }
    }

    pub fn set(&mut self, key: Ref<ObjString>, value: Value) -> bool {
        if self.count as f32 + 1.0 > self.capacity as f32 * TABLE_MAX_LOAD {
            self.grow_pointer();
        }

        let entry = unsafe { &mut *find_entry(self.entries, self.capacity, key) };
        let is_new_key = entry.key.is_none();
        if is_new_key && entry.value.is_nil() {
            self.count += 1;
        }

        entry.key = Some(key);
        entry.value = value;
        is_new_key
    }

    pub fn delete(&mut self, key: Ref<ObjString>) -> bool {
        if self.count == 0 {
            return false;
        }

        let entry = unsafe { &mut *find_entry(self.entries, self.capacity, key) };
        if entry.key.is_none() {
            return false;
        }

        entry.key = None;
        entry.value = true.into();

        true
    }

    pub fn add_all(&mut self, from: *const Table) {
        let from = unsafe { &*from };
        for i in 0..from.capacity {
            let entry = unsafe { &*from.entries.offset(i as isize) };
            if let Some(key) = entry.key {
                self.set(key, entry.value);
            }
        }
    }

    pub fn find_string(&mut self, chars: &[u8], hash: u32) -> Option<Ref<ObjString>> {
        if self.count == 0 {
            return None;
        }

        let mut index = hash & ((self.capacity - 1) as u32);
        loop {
            let entry = unsafe { &mut *self.entries.offset(index as isize) };
            match entry.key {
                None => {
                    if entry.value.is_nil() {
                        return None;
                    }
                }
                Some(key_ref) => {
                    let key: &ObjString = key_ref.value();
                    if key.chars.count() == chars.len()
                        && key.hash == hash
                        && &key.chars[0..key.chars.count()] == chars
                    {
                        return Some(key_ref);
                    }
                }
            }

            index = (index + 1) & ((self.capacity - 1) as u32);
        }
    }


    // TODO: impl Drop
    pub fn free(&mut self) {
        unsafe {
            memory::reallocate(self.entries, self.capacity, 0);
        }
    }
}

fn find_entry(entries: *mut Entry, capacity: usize, key_ref: Ref<ObjString>) -> *mut Entry {
    let key = key_ref.value();

    let mut index = key.hash & ((capacity - 1) as u32);
    let mut tombstone = ptr::null_mut();

    loop {
        let entry = unsafe { &mut *entries.add(index as usize) };
        match entry.key {
            None => {
                if entry.value.is_nil() {
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
            }
            Some(entry_key) => {
                if entry_key.has_same_ptr_as(&key_ref) {
                    return entry;
                }
            }
        }

        index = (index + 1) & ((capacity - 1) as u32);
    }
}

impl Index<Range<usize>> for Table {
    type Output = [Entry];

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
        unsafe { slice::from_raw_parts(self.entries.add(index.start), index.end - index.start) }
    }
}

impl Index<RangeFull> for Table {
    type Output = [Entry];

    fn index(&self, _index: RangeFull) -> &Self::Output {
        &self[0..self.count]
    }
}
