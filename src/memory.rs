use std::{
    alloc, mem,
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

/// just realloc
/// Guaranteed to not return a null pointer if new_count > 0 and old_count != new_count
///
/// # Safety
///
/// * pointer must either have been allocated by this function, or be null
///
/// * old_count must be the same value new_count was when the memory referenced by
/// pointer was allocated
///
/// * "new_size, when rounded up to the nearest multiple of layout.align(), must
/// not overflow (i.e., the rounded value must be less than usize::MAX)."
///
/// # Errors
/// TODO
pub unsafe fn reallocate<T>(pointer: *mut T, old_count: usize, new_count: usize) -> *mut T {
    if cfg!(feature = "debug_log_gc") {
        println!(
            "---> reallocate: [ pointer = {:?}, old_count = {}, new_count = {}, T = {} ]",
            pointer,
            old_count,
            new_count,
            std::any::type_name::<T>(),
        );
    }

    assert!(mem::size_of::<T>() > 0, "Can't allocate 0-sized types.");

    if old_count == new_count {
        if cfg!(feature = "debug_log_gc") {
            println!("<--- reallocate: [ {:?} ]", pointer);
        }
        return pointer;
    }

    let old_size = mem::size_of::<T>() * old_count;
    let new_size = mem::size_of::<T>() * new_count;
    let align = mem::align_of::<T>();

    // TODO: SAFETY: NOT THREAD-SAFE
    let gc: &GarbageCollector = todo!("&mut GC");

    assert!(
        gc.bytes_allocated >= old_size,
        "Can't have allocated {} bytes if only {} bytes are allocated so far.",
        old_size,
        gc.bytes_allocated
    );
    gc.bytes_allocated -= old_size;
    gc.bytes_allocated += new_size;

    if new_size > old_size {
        if cfg!(feature = "debug_stress_gc") || gc.bytes_allocated > gc.next_gc {
            // TODO: SAFETY: WHAT ARE THE PRE-CONDITIONS, IF ANY, FOR collect_garbage?
            gc.collect_garbage();
        }
    }

    let result;

    if pointer == ptr::null_mut() && new_count > 0 {
        // Case 1: new allocation
        let (layout, offset) = alloc::Layout::new::<T>()
            .repeat(new_count)
            .expect("new_count should not be big enough for a 64-bit integer to overflow");
        assert!(
            mem::size_of::<T>() == offset,
            "Array offset should be same value as type size in bytes."
        );

        // SAFETY: layout has non-zero size because T is asserted to have >0 size and
        // new_count >0 in this branch.
        result = alloc::alloc(layout);
        assert!(
            result != ptr::null_mut(),
            "reallocate(pointer = {:?}, old_size = {}, new_size = {}, align = {}) -> {:?}",
            pointer,
            old_size,
            new_size,
            align,
            result
        );
    } else {
        let (layout, _offset) = alloc::Layout::new::<T>()
            .repeat(old_count)
            .expect("old_count should not be big enough for a 64-bit integer to overflow");

        if new_count == 0 {
            // Case 2: Deallocation
            // SAFETY: pointer being allocated by this allocator and layout being the same
            // are covered by the preconditions of reallocate
            alloc::dealloc(pointer as *mut u8, layout);
            result = ptr::null_mut();
        } else {
            // Case 3: Realloc
            // SAFETY: pointer being allocated by this allocator and layout being the same
            // are covered by the preconditions of reallocate.
            // new_size is non-zero because T is asserted to be >0 and new_count >0 in this branch.
            // overflow condition is tentatively written as a precond for reallocate too, though I
            // would like to change that TODO TODO TODO
            result = alloc::realloc(pointer as *mut u8, layout, new_size);
            assert!(
                result != ptr::null_mut(),
                "reallocate(pointer = {:?}, old_size = {}, new_size = {}, align = {}) -> {:?}",
                pointer,
                old_size,
                new_size,
                align,
                result
            );
        }
    };

    #[cfg(feature = "debug_log_gc")]
    {
        println!("<--- reallocate: [ {:?} ]", result);
    }
    result as *mut T
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
    let object: *mut RefStorage<T> = unsafe { reallocate(ptr::null_mut(), 0, 1) };

    unsafe {
        let header = &mut (*object).header;
        header.ty = ty;
        header.is_marked = false;

        todo!("
        let current_head = GC.objects;
        GC.objects = header;
        header.next = current_head;");

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
    bytes_allocated: usize,
    next_gc: usize,
    objects: *mut GcHeader,
    gray_stack: Vec<*mut GcHeader>,
}

impl GarbageCollector {
    pub const fn new() -> Self {
        GarbageCollector {
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
            objects: ptr::null_mut(),
            gray_stack: vec![],
        }
    }
    

    unsafe fn collect_garbage(&mut self) {
        #[cfg(feature = "debug_log_gc")]
        {
            println!("-- gc begin");
        }
        #[allow(unused_variables)]
        let before = self.bytes_allocated;
    
        self.mark_roots();
        self.trace_references();
        remove_white(&mut self.interner.strings);
        self.sweep();
    
        self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;
    
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

    unsafe fn mark_roots(&mut self) {
        let vm: &VM = todo!("&mut *self.vm");

        let mut slot = vm.stack.as_mut_ptr();
        while slot < vm.stack_top {
            (*slot).as_erased_ref().map(|it| self.mark_object(it));
            slot = slot.add(1);
        }
    
        for i in 0..vm.frame_count {
            self.mark_object(vm.frames[i as usize].closure);
        }
    
        let mut upvalue = vm.open_upvalues;
        while let Some(uv) = upvalue {
            self.mark_object(uv);
            upvalue = uv.value().next;
        }
    
        todo!("abc");
        // self.mark_table(&mut vm.globals);
        // if self.parser != ptr::null_mut() {
        //     (*self.parser).mark_compiler_roots();
        // }
        self.mark_object(vm.init_string);
    }

    unsafe fn trace_references(&mut self) {
        while let Some(object) = self.gray_stack.pop() {
            self.blacken_object(object);
        }
    }
    
    unsafe fn sweep(&mut self) {
        let mut previous = ptr::null_mut();
        let mut object = self.objects;
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
                    self.objects = object;
                }
    
                free_object(unreached);
            }
        }
    }

    pub unsafe fn free_objects(&self) {
        let mut object = self.objects;
        while object != ptr::null_mut() {
            let next = (*object).next;
            free_object(object);
            object = next;
        }
    }

    // TODO: ACTUALLY THIS FUNCTION IS DEFINITELY 'unsafe' BUT I
    // CAN'T BE BOTHERED TO CHANGE THAT IN THE MIDDLE OF A BIG REFACTOR
    fn mark_header(&mut self, header: *mut GcHeader) {
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
            self.gray_stack.push(header);
        }
    }

    fn mark_object<T>(&mut self, mut object: Ref<T>) {
        self.mark_header(object.header_mut());
    }
    
    fn mark_array(&mut self, array: *mut Array<Value>) {
        for elem in unsafe { &(*array)[..] } {
            elem.as_erased_ref().map(|it| self.mark_object(it));
        }
    }

    
    unsafe fn blacken_object(&mut self, header: *mut GcHeader) {
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
                    (*val).as_erased_ref().map(|it| self.mark_object(it));
                }
            }
            ObjType::Closure => {
                let mut closure: Ref<ObjClosure> = Ref::from_header(header);
                self.mark_object(closure.value_mut().function);
                for i in 0..closure.value().upvalues.count() {
                    if let Some(uv) = closure.value().upvalues[i] {
                        self.mark_object(uv);
                    }
                }
            }
            ObjType::Function => {
                let mut function: Ref<ObjFunction> = Ref::from_header(header);
                if let Some(name) = function.value().name {
                    self.mark_object(name);
                }
                self.mark_array(&mut function.value_mut().chunk.constants);
            }
            ObjType::Upvalue => {
                let upvalue: Ref<ObjUpvalue> = Ref::from_header(header);
                upvalue.value().closed.as_erased_ref().map(|it| self.mark_object(it));
            }
            ObjType::Class => {
                let mut class: Ref<ObjClass> = Ref::from_header(header);
                self.mark_object(class.value_mut().name);
                self.mark_table(&mut class.value_mut().methods);
            }
            ObjType::Instance => {
                let mut instance: Ref<ObjInstance> = Ref::from_header(header);
                self.mark_object(instance.value_mut().class);
                self.mark_table(&mut instance.value_mut().fields);
            }
            ObjType::BoundMethod => {
                let mut bound: Ref<ObjBoundMethod> = Ref::from_header(header);
                bound.value().receiver.as_erased_ref().map(|it| self.mark_object(it));
                self.mark_object(bound.value_mut().method);
            }
            ObjType::Native | ObjType::String => {}
        }
    }

    pub fn mark_table(&mut self, table: &mut Table) {
        for entry in &table[..] {
            entry.key().map(|it| self.mark_object(it));
            entry.value().as_erased_ref().map(|it| self.mark_object(it));
        }
    }
}

unsafe fn free<T>(rf: Ref<T>) {
    reallocate(rf.inner.as_ptr(), 1, 0);
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

// // TODO: should this go in memory.rs or not?
// pub struct StringInterner {
//     tracking: Option<Ref<ObjString>>,
//     strings: Table,
// }

// impl StringInterner {
//     pub fn new() -> Self {
//         Self { tracking: None, strings: Table::new() }
//     }

//     pub fn intern(&mut self, string: Ref<ObjString>) {
//         self.tracking = Some(string);
//         self.strings.set(string, Value::Nil);
//         self.tracking = None;
//     }

//     pub fn find(&self, chars: &[u8], hash: u32) -> Option<Ref<ObjString>> {
//         self.strings.find_string(chars, hash)
//     }
// }