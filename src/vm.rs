use std::{
    mem, ptr,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::{chunk::OpCode, compiler::compile, memory::free_objects, object::{NativeFn, Obj, ObjClass, ObjClosure, ObjString, ObjType, ObjUpvalue, as_bound_method, as_class, as_closure, as_function, as_instance, as_native, as_string, copy_string, is_class, is_instance, is_string, new_bound_method, new_class, new_closure, new_instance, new_list, new_native, new_upvalue, obj_type, take_string}, table::{free_table, init_table, table_add_all, table_delete, table_get, table_set, Table}, value::{
        as_bool, as_number, bool_val, is_bool, is_nil, is_number, is_obj, NIL_VAL, number_val,
        obj_val, print_value, values_equal, Value,
    }};

#[cfg(feature = "debug_trace_execution")]
use crate::debug::disassemble_instruction;

pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * u8::MAX as usize;

#[derive(Clone, Copy)]
pub struct CallFrame {
    pub closure: *mut ObjClosure,
    pub ip: *mut u8,
    pub slots: *mut Value,
}

pub struct VM {
    pub frames: [CallFrame; FRAMES_MAX],
    pub frame_count: i32,
    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,
    pub globals: Table,
    pub strings: Table,
    pub init_string: *mut ObjString,
    pub open_upvalues: *mut ObjUpvalue,
    pub bytes_allocated: usize,
    pub next_gc: usize,
    pub objects: *mut Obj,
    pub gray_stack: Vec<*mut Obj>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

const ZERO_CALL_FRAME: CallFrame = CallFrame {
    closure: ptr::null_mut(),
    ip: ptr::null_mut(),
    slots: ptr::null_mut(),
};

#[allow(non_upper_case_globals)]
pub static mut vm: VM = VM {
    frames: [ZERO_CALL_FRAME; FRAMES_MAX],
    frame_count: 0,
    stack: [NIL_VAL; STACK_MAX],
    stack_top: ptr::null_mut(),
    globals: Table {
        count: 0,
        capacity: 0,
        entries: ptr::null_mut(),
    },
    strings: Table {
        count: 0,
        capacity: 0,
        entries: ptr::null_mut(),
    },
    init_string: ptr::null_mut(),
    open_upvalues: ptr::null_mut(),
    bytes_allocated: 0,
    next_gc: 0,
    objects: ptr::null_mut(),
    gray_stack: vec![],
};

unsafe fn clock_native(_arg_count: i32, _args: *mut Value) -> Value {
    let clock = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_secs_f64();
    return number_val(clock);
}

unsafe fn reset_stack() {
    vm.stack_top = vm.stack.as_mut_ptr();
    vm.frame_count = 0;
    vm.open_upvalues = ptr::null_mut();
}

unsafe fn _runtime_error() {
    for i in (0..vm.frame_count).rev() {
        let frame = &mut vm.frames[i as usize];
        let function = (*frame.closure).function;
        let instruction = frame.ip.sub((*function).chunk.code as usize).sub(1) as usize;
        eprint!("[line {}] in ", *(*function).chunk.lines.add(instruction));
        if (*function).name == ptr::null_mut() {
            eprintln!("script");
        } else {
            let name = &*(*function).name;
            eprintln!(
                "{}()",
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    name.chars,
                    name.length as usize
                ))
            );
        }
    }
}

macro_rules! runtime_error {
    ($($args:expr),*) => {
        eprintln!($($args),*);
        _runtime_error();
    };
}

unsafe fn define_native(name: &str, function: NativeFn) {
    push(obj_val(
        copy_string(name.as_ptr(), name.len() as i32) as *mut Obj
    ));
    push(obj_val(new_native(function) as *mut Obj));
    table_set(&mut vm.globals, as_string(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

pub unsafe fn init_vm() {
    reset_stack();
    vm.objects = ptr::null_mut();
    vm.bytes_allocated = 0;
    vm.next_gc = 1024 * 1024;

    vm.gray_stack = vec![];

    init_table(&mut vm.globals);
    init_table(&mut vm.strings);

    vm.init_string = ptr::null_mut();
    vm.init_string = copy_string("init".as_ptr(), 4);

    define_native("clock", clock_native);
}

pub unsafe fn free_vm() {
    free_table(&mut vm.globals);
    free_table(&mut vm.strings);
    vm.init_string = ptr::null_mut();
    free_objects();
}

pub unsafe fn interpret(source: &str) -> InterpretResult {
    let function = compile(source);
    if function == ptr::null_mut() {
        return InterpretResult::CompileError;
    }

    push(obj_val(function as *mut Obj));
    let closure = new_closure(function);
    pop();
    push(obj_val(closure as *mut Obj));
    call(closure, 0);

    let result = run();

    result
}

pub unsafe fn push(value: Value) {
    *vm.stack_top = value;
    vm.stack_top = vm.stack_top.add(1);
}

pub unsafe fn pop() -> Value {
    vm.stack_top = vm.stack_top.sub(1);
    *vm.stack_top
}

unsafe fn peek(distance: isize) -> Value {
    *vm.stack_top.offset(-1 - distance)
}

unsafe fn call(closure: *mut ObjClosure, arg_count: i32) -> bool {
    if arg_count != (*(*closure).function).arity {
        runtime_error!(
            "Expected {} arguments but got {}.",
            (*(*closure).function).arity,
            arg_count
        );
        return false;
    }

    if vm.frame_count as usize >= FRAMES_MAX {
        runtime_error!("Stack overflow.");
        return false;
    }

    let frame = &mut vm.frames[vm.frame_count as usize];
    vm.frame_count += 1;
    frame.closure = closure;
    frame.ip = (*(*closure).function).chunk.code;
    frame.slots = vm.stack_top.sub(arg_count as usize).sub(1);
    true
}

unsafe fn call_value(callee: Value, arg_count: i32) -> bool {
    if is_obj(callee) {
        match obj_type(callee) {
            ObjType::BoundMethod => {
                let bound = as_bound_method(callee);
                *vm.stack_top.offset(-arg_count as isize - 1) = (*bound).receiver;
                return call((*bound).method, arg_count);
            }
            ObjType::Class => {
                let class = as_class(callee);
                *vm.stack_top.offset(-arg_count as isize - 1) =
                    obj_val(new_instance(class) as *mut Obj);
                let mut initializer = NIL_VAL;
                if table_get(&mut (*class).methods, vm.init_string, &mut initializer) {
                    return call(as_closure(initializer), arg_count);
                } else if arg_count != 0 {
                    runtime_error!("Expected 0 arguments but got {}.", arg_count);
                    return false;
                }
                return true;
            }
            ObjType::Closure => return call(as_closure(callee), arg_count),
            ObjType::Native => {
                let native = as_native(callee);
                let result = (native)(arg_count, vm.stack_top.sub(arg_count as usize));
                vm.stack_top = vm.stack_top.sub(arg_count as usize + 1);
                push(result);
                return true;
            }
            _ => {}
        }
    }
    runtime_error!("Can only call functions and classes.");
    false
}

unsafe fn invoke_from_class(class: *mut ObjClass, name: *mut ObjString, arg_count: i32) -> bool {
    let mut method = NIL_VAL;
    if !table_get(&mut (*class).methods, name, &mut method) {
        runtime_error!("Undefined property '{}'.", "name->chars"); // todo
        return false;
    }
    call(as_closure(method), arg_count)
}

unsafe fn invoke(name: *mut ObjString, arg_count: i32) -> bool {
    let receiver = peek(arg_count as isize);

    if !is_instance(receiver) {
        runtime_error!("Only instances have methods.");
        return false;
    }

    let instance = as_instance(receiver);

    let mut value = NIL_VAL;
    if table_get(&mut (*instance).fields, name, &mut value) {
        *vm.stack_top.offset(-arg_count as isize - 1) = value;
        return call_value(value, arg_count);
    }

    invoke_from_class((*instance).class, name, arg_count)
}

unsafe fn bind_method(class: *mut ObjClass, name: *mut ObjString) -> bool {
    let mut method = NIL_VAL;
    if !table_get(&mut (*class).methods, name, &mut method) {
        runtime_error!("Undefined property '{}'.", "name->chars");
        return false;
    }

    let bound = new_bound_method(peek(0), as_closure(method));
    pop();
    push(obj_val(bound as *mut Obj));
    true
}

unsafe fn capture_upvalue(local: *mut Value) -> *mut ObjUpvalue {
    let mut prev_upvalue = ptr::null_mut();
    let mut upvalue = vm.open_upvalues;
    while upvalue != ptr::null_mut() && (*upvalue).location > local {
        prev_upvalue = upvalue;
        upvalue = (*upvalue).next;
    }
    if upvalue != ptr::null_mut() && (*upvalue).location == local {
        return upvalue;
    }

    let created_upvalue = new_upvalue(local);
    (*created_upvalue).next = upvalue;

    if prev_upvalue == ptr::null_mut() {
        vm.open_upvalues = created_upvalue;
    } else {
        (*prev_upvalue).next = created_upvalue;
    }

    return created_upvalue;
}

unsafe fn close_upvalues(last: *mut Value) {
    while vm.open_upvalues != ptr::null_mut() && (*vm.open_upvalues).location >= last {
        let upvalue = vm.open_upvalues;
        (*upvalue).closed = *(*upvalue).location;
        (*upvalue).location = &mut (*upvalue).closed;
        vm.open_upvalues = (*upvalue).next;
    }
}

unsafe fn define_method(name: *mut ObjString) {
    let method = peek(0);
    let class = as_class(peek(1));
    table_set(&mut (*class).methods, name, method);
    pop();
}

unsafe fn is_falsey(value: Value) -> bool {
    is_nil(value) || (is_bool(value) && !as_bool(value))
}

unsafe fn concatenate() {
    let b = &*as_string(peek(0));
    let a = &*as_string(peek(1));

    let length = a.length + b.length;
    let chars = allocate!(u8, length + 1);
    ptr::copy_nonoverlapping(a.chars, chars, a.length as usize);
    ptr::copy_nonoverlapping(b.chars, chars.offset(a.length as isize), b.length as usize);
    *chars.offset(length as isize) = 0;

    let result = take_string(chars, length);
    pop();
    pop();
    push(obj_val(mem::transmute(result)));
}

unsafe fn run() -> InterpretResult {
    let mut frame = &mut vm.frames[vm.frame_count as usize - 1];

    macro_rules! read_byte {
        () => {{
            let v = *frame.ip;
            frame.ip = frame.ip.add(1);
            v
        }};
    }
    macro_rules! read_short {
        () => {{
            frame.ip = frame.ip.offset(2);
            (*frame.ip.offset(-2) as u16) << 8 | *frame.ip.offset(-1) as u16
        }};
    }
    macro_rules! read_constant {
        () => {
            *(*(*frame.closure).function)
                .chunk
                .constants
                .values
                .add(read_byte!() as usize)
        };
    }
    macro_rules! read_string {
        () => {
            as_string(read_constant!())
        };
    }
    macro_rules! binary_op {
        ($value_type:ident, $op:tt) => {{
            if !is_number(peek(0)) || !is_number(peek(1)) {
                runtime_error!("Operands must be numbers.");
                return InterpretResult::RuntimeError;
            }
            let b = as_number(pop());
            let a = as_number(pop());
            push($value_type(a $op b));
        }}
    }

    loop {
        #[cfg(feature = "debug_trace_execution")]
        {
            print!("          ");
            let mut slot = vm.stack.as_ptr();
            while slot < vm.stack_top {
                print!("[ ");
                print_value(*slot);
                print!(" ]");
                slot = slot.add(1);
            }
            println!("");
            disassemble_instruction(
                &mut (*(*frame.closure).function).chunk,
                frame
                    .ip
                    .sub((*(*frame.closure).function).chunk.code as usize) as i32,
            );
        }

        let instruction = read_byte!();
        match instruction {
            i if i == OpCode::Constant as u8 => {
                let constant = read_constant!();
                push(constant);
            }
            i if i == OpCode::Nil as u8 => push(NIL_VAL),
            i if i == OpCode::True as u8 => push(bool_val(true)),
            i if i == OpCode::False as u8 => push(bool_val(false)),
            i if i == OpCode::List as u8 => {
                let count = read_byte!();
                // TODO: vecs won't trigger gc... 
                // what if i run out of memory while doing this operation?
                let mut items = vec![]; 
                for i in 0..count {
                    let val = peek((count - i - 1).into());
                    items.push(val);
                }
                for _i in 0..count {
                    pop();
                }
                let list = new_list();
                (*list).items = items;
                push(obj_val(list as *mut Obj));
            }
            i if i == OpCode::Pop as u8 => {
                pop();
            }
            i if i == OpCode::GetLocal as u8 => {
                let slot = read_byte!();
                push(*frame.slots.offset(slot as isize));
            }
            i if i == OpCode::SetLocal as u8 => {
                let slot = read_byte!();
                *frame.slots.offset(slot as isize) = peek(0);
            }
            i if i == OpCode::GetGlobal as u8 => {
                let name = read_string!();
                let mut value = NIL_VAL; // @todo uninitialized
                if !table_get(&mut vm.globals, name, &mut value) {
                    runtime_error!(
                        "Undefined variable '{}'.",
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            (*name).chars,
                            (*name).length as usize
                        ))
                    );
                    return InterpretResult::RuntimeError;
                }
                push(value);
            }
            i if i == OpCode::DefineGlobal as u8 => {
                let name = read_string!();
                table_set(&mut vm.globals, name, peek(0));
                pop();
            }
            i if i == OpCode::SetGlobal as u8 => {
                let name = read_string!();
                if table_set(&mut vm.globals, name, peek(0)) {
                    // true means key wasn't in table
                    table_delete(&mut vm.globals, name);
                    runtime_error!(
                        "Undefined variable '{}'.",
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            (*name).chars,
                            (*name).length as usize
                        ))
                    );
                    return InterpretResult::RuntimeError;
                }
            }
            i if i == OpCode::GetUpvalue as u8 => {
                let slot = read_byte!();
                push(*(**(*(*frame).closure).upvalues.offset(slot as isize)).location);
            }
            i if i == OpCode::SetUpvalue as u8 => {
                let slot = read_byte!();
                *(**(*(*frame).closure).upvalues.offset(slot as isize)).location = peek(0);
            }
            i if i == OpCode::GetProperty as u8 => {
                if !is_instance(peek(0)) {
                    runtime_error!("Only instances have properties.");
                    return InterpretResult::RuntimeError;
                }

                let instance = as_instance(peek(0));
                let name = read_string!();

                let mut value = NIL_VAL;
                if table_get(&mut (*instance).fields, name, &mut value) {
                    pop();
                    push(value);
                } else if !bind_method((*instance).class, name) {
                    return InterpretResult::RuntimeError;
                }
            }
            i if i == OpCode::SetProperty as u8 => {
                if !is_instance(peek(1)) {
                    runtime_error!("Only instances have fields.");
                    return InterpretResult::RuntimeError;
                }

                let instance = as_instance(peek(1));
                table_set(&mut (*instance).fields, read_string!(), peek(0));
                let value = pop();
                pop();
                push(value);
            }
            i if i == OpCode::GetSuper as u8 => {
                let name = read_string!();
                let superclass = as_class(pop());

                if !bind_method(superclass, name) {
                    return InterpretResult::RuntimeError;
                }
            }
            i if i == OpCode::Equal as u8 => {
                let b = pop();
                let a = pop();
                push(bool_val(values_equal(a, b)));
            }
            i if i == OpCode::Greater as u8 => binary_op!(bool_val, >),
            i if i == OpCode::Less as u8 => binary_op!(bool_val, <),
            i if i == OpCode::Not as u8 => push(bool_val(is_falsey(pop()))),
            i if i == OpCode::Negate as u8 => {
                if !is_number(peek(0)) {
                    runtime_error!("Operand must be a number.");
                    return InterpretResult::RuntimeError;
                }
                push(number_val(-as_number(pop())));
            }
            i if i == OpCode::Add as u8 => {
                if is_string(peek(0)) && is_string(peek(1)) {
                    concatenate();
                } else if is_number(peek(0)) && is_number(peek(1)) {
                    let b = as_number(pop());
                    let a = as_number(pop());
                    push(number_val(a + b));
                } else {
                    runtime_error!("Operands must be two numbers or two strings.");
                    return InterpretResult::RuntimeError;
                }
            }
            i if i == OpCode::Subtract as u8 => binary_op!(number_val, -),
            i if i == OpCode::Multiply as u8 => binary_op!(number_val, *),
            i if i == OpCode::Divide as u8 => binary_op!(number_val, /),
            i if i == OpCode::Print as u8 => {
                print_value(pop());
                println!("");
            }
            i if i == OpCode::Jump as u8 => {
                let offset = read_short!();
                frame.ip = frame.ip.offset(offset as isize);
            }
            i if i == OpCode::JumpIfFalse as u8 => {
                let offset = read_short!();
                if is_falsey(peek(0)) {
                    frame.ip = frame.ip.offset(offset as isize);
                }
            }
            i if i == OpCode::Loop as u8 => {
                let offset = read_short!();
                frame.ip = frame.ip.offset(-(offset as isize));
            }
            i if i == OpCode::Call as u8 => {
                let arg_count = read_byte!();
                if !call_value(peek(arg_count.into()), arg_count.into()) {
                    return InterpretResult::RuntimeError;
                }
                frame = &mut vm.frames[vm.frame_count as usize - 1];
            }
            i if i == OpCode::Invoke as u8 => {
                let method = read_string!();
                let arg_count = read_byte!();
                if !invoke(method, arg_count as i32) {
                    return InterpretResult::RuntimeError;
                }
                frame = &mut vm.frames[vm.frame_count as usize - 1];
            }
            i if i == OpCode::SuperInvoke as u8 => {
                let method = read_string!();
                let arg_count = read_byte!();
                let superclass = as_class(pop());
                if !invoke_from_class(superclass, method, arg_count as i32) {
                    return InterpretResult::RuntimeError;
                }
                frame = &mut vm.frames[vm.frame_count as usize - 1];
            }
            i if i == OpCode::Closure as u8 => {
                let function = as_function(read_constant!());
                let closure = new_closure(function);
                push(obj_val(closure as *mut Obj));
                for i in 0..(*closure).upvalue_count {
                    let is_local = read_byte!();
                    let index = read_byte!();
                    if is_local == 1 {
                        *(*closure).upvalues.offset(i as isize) =
                            capture_upvalue((*frame).slots.offset(index as isize));
                    } else {
                        *(*closure).upvalues.offset(i as isize) =
                            *(*(*frame).closure).upvalues.offset(index as isize);
                    }
                }
            }
            i if i == OpCode::CloseUpvalue as u8 => {
                close_upvalues(vm.stack_top.sub(1));
                pop();
            }
            i if i == OpCode::Return as u8 => {
                let result = pop();
                close_upvalues((*frame).slots);
                vm.frame_count -= 1;
                if vm.frame_count == 0 {
                    pop();
                    return InterpretResult::Ok;
                }
                vm.stack_top = frame.slots;
                push(result);
                frame = &mut vm.frames[vm.frame_count as usize - 1];
            }
            i if i == OpCode::Class as u8 => {
                push(obj_val(new_class(read_string!()) as *mut Obj));
            }
            i if i == OpCode::Inherit as u8 => {
                let superclass = peek(1);
                if !is_class(superclass) {
                    runtime_error!("Superclass must be a class.");
                    return InterpretResult::RuntimeError;
                }
                let subclass = as_class(peek(0));
                table_add_all(
                    &mut (*as_class(superclass)).methods,
                    &mut (*subclass).methods,
                );
                pop();
            }
            i if i == OpCode::Method as u8 => {
                define_method(read_string!());
            }
            _ => break,
        }
    }

    InterpretResult::RuntimeError
}
