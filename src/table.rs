use std::{
    ptr::{self, null_mut},
    slice,
};

use crate::{memory::{mark_object, mark_value}, object::{Obj, ObjString}, value::{bool_val, is_nil, Value, NIL_VAL}, vm::VM};

const TABLE_MAX_LOAD: f32 = 0.75;

#[derive(Debug)]
pub struct Table {
    pub count: i32,
    pub capacity: i32,
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

unsafe fn find_entry(entries: *mut Entry, capacity: i32, key: *mut ObjString) -> *mut Entry {
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

unsafe fn adjust_capacity(table: *mut Table, capacity: i32) {
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
        let capacity = grow_capacity!(table.capacity);
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

pub unsafe fn table_find_string(
    table: *mut Table,
    chars: *const u8,
    length: i32,
    hash: u32,
) -> *mut ObjString {
    let table = &mut *table;
    if table.count == 0 {
        return ptr::null_mut();
    }

    let mut index = hash & ((table.capacity - 1) as u32);
    loop {
        let entry = &mut *table.entries.offset(index as isize);
        if entry.key == null_mut() {
            if is_nil(entry.value) {
                return ptr::null_mut();
            }
        } else {
            let key = &mut *entry.key;
            if key.length == length
                && key.hash == hash
                && slice::from_raw_parts(key.chars, length as usize)
                    == slice::from_raw_parts(chars, length as usize)
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

pub unsafe fn mark_table(table: *mut Table, vm: &mut VM) {
    for i in 0..(*table).capacity {
        let entry = (*table).entries.offset(i as isize);
        mark_object(vm, (*entry).key as *mut Obj);
        mark_value(vm, (*entry).value);
    }
}
