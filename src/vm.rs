use std::{
    ptr,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::{
    array::{Array, Table},
    chunk::OpCode,
    compiler::compile,
    memory::{free_objects, ObjType, Ref},
    object::{
        as_bound_method, as_class, as_closure, as_function, as_instance, as_list, as_native,
        as_string, copy_string, is_class, is_instance, is_list, is_string, new_bound_method,
        new_class, new_closure, new_instance, new_list, new_native, new_upvalue, take_string,
        NativeFn, ObjClass, ObjClosure, ObjString, ObjUpvalue,
    },
    value::Value,
};

#[cfg(feature = "debug_trace_execution")]
use crate::debug::disassemble_instruction;

pub const FRAMES_MAX: usize = 64;
pub const STACK_MAX: usize = FRAMES_MAX * u8::MAX as usize;

#[derive(Clone, Copy)]
pub struct CallFrame {
    pub closure: Ref<ObjClosure>,
    pub ip: usize,
    pub slots: *mut Value,
}

pub struct VM {
    pub frames: [CallFrame; FRAMES_MAX],
    pub frame_count: i32,
    pub stack: [Value; STACK_MAX],
    pub stack_top: *mut Value,
    pub globals: Table,
    pub init_string: Ref<ObjString>,
    pub open_upvalues: Option<Ref<ObjUpvalue>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

const ZERO_CALL_FRAME: CallFrame = CallFrame {
    closure: Ref::dangling(),
    ip: 0,
    slots: ptr::null_mut(),
};

unsafe fn clock_native(_arg_count: i32, _args: *mut Value) -> Value {
    let clock = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0))
        .as_secs_f64();
    return clock.into();
}

unsafe fn sqrt_native(arg_count: i32, args: *mut Value) -> Value {
    if arg_count != 1 || (*args).as_number().is_none() {
        panic!("sqrt() only takes 1 numeric argument.");
    }
    return (*args).as_number().unwrap().sqrt().into();
}

unsafe fn _runtime_error(vm: &mut VM) {
    for i in (0..vm.frame_count).rev() {
        let frame = &mut vm.frames[i as usize];
        let function = frame.closure.value().function;
        let instruction = frame.ip - 1;
        eprint!("[line {}] in ", function.value().chunk.lines[instruction]);
        match function.value().name {
            None => eprintln!("script"),
            Some(name) => {
                eprintln!(
                    "{}()",
                    std::str::from_utf8_unchecked(&name.value().chars[..])
                )
            }
        }
    }
}

macro_rules! runtime_error {
    ($vm:expr, $($args:expr),*) => {
        eprintln!($($args),*);
        _runtime_error($vm);
    };
}

impl VM {
    pub unsafe fn new() -> VM {
        let vm: VM = VM {
            frames: [ZERO_CALL_FRAME; FRAMES_MAX],
            frame_count: 0,
            stack: [Value::NIL; STACK_MAX],
            stack_top: ptr::null_mut(),
            globals: Table::new(),
            init_string: Ref::dangling(),
            open_upvalues: None,
        };
        vm
    }

    pub unsafe fn init(&mut self) {
        let vm = self;
        vm.reset_stack();

        vm.globals = Table::new();

        vm.open_upvalues = None;
        vm.init_string = copy_string("init".as_bytes());

        vm.define_native("clock", clock_native);
        vm.define_native("sqrt", sqrt_native);
    }

    unsafe fn reset_stack(&mut self) {
        self.stack_top = self.stack.as_mut_ptr();
        self.frame_count = 0;
        self.open_upvalues = None;
    }

    unsafe fn define_native(&mut self, name: &str, function: NativeFn) {
        self.push(copy_string(name.as_bytes()).into());
        self.push(new_native(function).into());
        self.globals.set(as_string(self.stack[0]), self.stack[1]);
        self.pop();
        self.pop();
    }

    pub unsafe fn push(&mut self, value: Value) {
        *self.stack_top = value;
        self.stack_top = self.stack_top.add(1);
    }

    pub unsafe fn pop(&mut self) -> Value {
        self.stack_top = self.stack_top.sub(1);
        *self.stack_top
    }

    unsafe fn peek(&mut self, distance: isize) -> Value {
        *self.stack_top.offset(-1 - distance)
    }

    unsafe fn call(&mut self, closure: Ref<ObjClosure>, arg_count: i32) -> bool {
        if arg_count != closure.value().function.value().arity {
            runtime_error!(
                self,
                "Expected {} arguments but got {}.",
                closure.value().function.value().arity,
                arg_count
            );
            return false;
        }

        if self.frame_count as usize >= FRAMES_MAX {
            runtime_error!(self, "Stack overflow.");
            return false;
        }

        let frame = &mut self.frames[self.frame_count as usize];
        self.frame_count += 1;
        frame.closure = closure;
        frame.ip = 0;
        frame.slots = self.stack_top.sub(arg_count as usize).sub(1);
        true
    }

    unsafe fn call_value(&mut self, callee: Value, arg_count: i32) -> bool {
        if let Some(c) = callee.as_erased_ref() {
            match c.ty() {
                ObjType::BoundMethod => {
                    let bound = as_bound_method(callee);
                    *self.stack_top.offset(-arg_count as isize - 1) = bound.value().receiver;
                    return self.call(bound.value().method, arg_count);
                }
                ObjType::Class => {
                    let mut class = as_class(callee);
                    *self.stack_top.offset(-arg_count as isize - 1) = new_instance(class).into();
                    if let Some(initializer) = class.value_mut().methods.get(self.init_string) {
                        return self.call(as_closure(initializer), arg_count);
                    } else if arg_count != 0 {
                        runtime_error!(self, "Expected 0 arguments but got {}.", arg_count);
                        return false;
                    }
                    return true;
                }
                ObjType::Closure => return self.call(as_closure(callee), arg_count),
                ObjType::Native => {
                    let native = as_native(callee);
                    let result = (native)(arg_count, self.stack_top.sub(arg_count as usize));
                    self.stack_top = self.stack_top.sub(arg_count as usize + 1);
                    self.push(result);
                    return true;
                }
                _ => {}
            }
        }

        runtime_error!(self, "Can only call functions and classes.");
        false
    }

    unsafe fn invoke_from_class(
        &mut self,
        mut class: Ref<ObjClass>,
        name: Ref<ObjString>,
        arg_count: i32,
    ) -> bool {
        let method = class.value_mut().methods.get(name);
        match method {
            Some(method) => self.call(as_closure(method), arg_count),
            None => {
                runtime_error!(self, "Undefined property '{}'.", "name->chars"); // todo
                false
            }
        }
    }

    unsafe fn invoke(&mut self, name: Ref<ObjString>, arg_count: i32) -> bool {
        let receiver = self.peek(arg_count as isize);

        if !is_instance(receiver) {
            runtime_error!(self, "Only instances have methods.");
            return false;
        }

        let mut instance = as_instance(receiver);

        if let Some(value) = instance.value_mut().fields.get(name) {
            *self.stack_top.offset(-arg_count as isize - 1) = value;
            return self.call_value(value, arg_count);
        }

        self.invoke_from_class(instance.value().class, name, arg_count)
    }

    unsafe fn bind_method(&mut self, mut class: Ref<ObjClass>, name: Ref<ObjString>) -> bool {
        let method = match class.value_mut().methods.get(name) {
            None => {
                runtime_error!(self, "Undefined property '{}'.", "name->chars");
                return false;
            }
            Some(method) => method,
        };

        let bound = new_bound_method(self.peek(0), as_closure(method));
        self.pop();
        self.push(bound.into());
        true
    }

    unsafe fn capture_upvalue(&mut self, local: *mut Value) -> Ref<ObjUpvalue> {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalues;
        while upvalue.is_some() && upvalue.unwrap().value().location > local {
            prev_upvalue = upvalue;
            upvalue = upvalue.unwrap().value().next;
        }
        if upvalue.is_some() && upvalue.unwrap().value().location == local {
            return upvalue.unwrap();
        }

        let mut created_upvalue = new_upvalue(local);
        created_upvalue.value_mut().next = upvalue;

        if prev_upvalue.is_none() {
            self.open_upvalues = Some(created_upvalue);
        } else {
            prev_upvalue.unwrap().value_mut().next = Some(created_upvalue);
        }

        return created_upvalue;
    }

    unsafe fn close_upvalues(&mut self, last: *mut Value) {
        while self.open_upvalues.is_some() && self.open_upvalues.unwrap().value().location >= last {
            let mut upvalue = self.open_upvalues.unwrap();
            upvalue.value_mut().closed = *upvalue.value().location;
            upvalue.value_mut().location = &mut upvalue.value_mut().closed;
            self.open_upvalues = upvalue.value().next;
        }
    }

    unsafe fn define_method(&mut self, name: Ref<ObjString>) {
        let method = self.peek(0);
        let mut class = as_class(self.peek(1));
        class.value_mut().methods.set(name, method);
        self.pop();
    }

    unsafe fn concatenate(&mut self) {
        let b = as_string(self.peek(0));
        let a = as_string(self.peek(1));

        // TODO TODO: Unnecessary extra allocation in "to_owned"
        let mut concatenated = a.value().chars[..].to_owned();
        concatenated.extend(&b.value().chars[..]);

        let array = Array::from(concatenated.as_slice());

        // // Previous code
        // let length = a.length + b.length;
        // let chars = allocate!(u8, length + 1);
        // ptr::copy_nonoverlapping(a.chars, chars, a.length as usize);
        // ptr::copy_nonoverlapping(b.chars, chars.offset(a.length as isize), b.length as usize);
        // *chars.offset(length as isize) = 0;

        let result = take_string(array);
        self.pop();
        self.pop();
        self.push(result.into());
    }

    unsafe fn run(&mut self) -> InterpretResult {
        let mut frame: *mut CallFrame = &mut self.frames[self.frame_count as usize - 1];

        macro_rules! read_byte {
            () => {{
                let v = (*frame).closure.value().function.value().chunk.code[(*frame).ip];
                (*frame).ip += 1;
                v
            }};
        }
        macro_rules! read_short {
            () => {{
                (*frame).ip += 2;
                let up = (*frame).closure.value().function.value().chunk.code[(*frame).ip - 2];
                let down = (*frame).closure.value().function.value().chunk.code[(*frame).ip - 1];
                (up as u16) << 8 | down as u16
            }};
        }
        macro_rules! read_constant {
            () => {
                (*frame).closure.value().function.value().chunk.constants[usize::from(read_byte!())]
            };
        }
        macro_rules! read_string {
            () => {
                as_string(read_constant!())
            };
        }
        macro_rules! binary_op {
            ($op:tt) => {{
                let shut_up_borrow_checker_0 = self.peek(0);
                let shut_up_borrow_checker_1 = self.peek(1);
                match (shut_up_borrow_checker_0.as_number(), shut_up_borrow_checker_1.as_number()) {
                    (Some(_), Some(_)) => {
                        let b = self.pop().as_number().unwrap();
                        let a = self.pop().as_number().unwrap();
                        self.push((a $op b).into());
                    }
                    _ => {
                        runtime_error!(self, "Operands must be numbers.");
                        return InterpretResult::RuntimeError;
                    }
                }
            }}
        }

        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                print!("          ");
                let mut slot = self.stack.as_ptr();
                while slot < self.stack_top {
                    print!("[ ");
                    print_value(*slot);
                    print!(" ]");
                    slot = slot.add(1);
                }
                println!("");
                disassemble_instruction(&mut (*(*(*frame).closure).function).chunk, (*frame).ip);
            }

            let instruction = read_byte!();
            match instruction {
                i if i == OpCode::Constant as u8 => {
                    let constant = read_constant!();
                    self.push(constant);
                }
                i if i == OpCode::Nil as u8 => self.push(Value::Nil),
                i if i == OpCode::True as u8 => self.push(true.into()),
                i if i == OpCode::False as u8 => self.push(false.into()),
                i if i == OpCode::List as u8 => {
                    let count = read_byte!();
                    // TODO: vecs won't trigger gc...
                    // what if i run out of memory while doing this operation?
                    let mut items = vec![];
                    for i in 0..count {
                        let val = self.peek((count - i - 1).into());
                        items.push(val);
                    }
                    for _i in 0..count {
                        self.pop();
                    }
                    let mut list = new_list();
                    list.value_mut().items = items;
                    self.push(list.into());
                }
                i if i == OpCode::Pop as u8 => {
                    self.pop();
                }
                i if i == OpCode::GetLocal as u8 => {
                    let slot = read_byte!();
                    self.push(*(*frame).slots.offset(slot as isize));
                }
                i if i == OpCode::SetLocal as u8 => {
                    let slot = read_byte!();
                    *(*frame).slots.offset(slot as isize) = self.peek(0);
                }
                i if i == OpCode::GetGlobal as u8 => {
                    let name = read_string!();
                    let value = match self.globals.get(name) {
                        None => {
                            runtime_error!(
                                self,
                                "Undefined variable '{}'.",
                                std::str::from_utf8_unchecked(&name.value().chars[..])
                            );
                            return InterpretResult::RuntimeError;
                        }
                        Some(value) => value,
                    };

                    self.push(value);
                }
                i if i == OpCode::DefineGlobal as u8 => {
                    let name = read_string!();
                    let shut_up_borrow_checker = self.peek(0);
                    self.globals.set(name, shut_up_borrow_checker);
                    self.pop();
                }
                i if i == OpCode::SetGlobal as u8 => {
                    let name = read_string!();

                    let shut_up_borrow_checker = self.peek(0);
                    if self.globals.set(name, shut_up_borrow_checker) {
                        // true means key wasn't in table
                        self.globals.delete(name);
                        runtime_error!(
                            self,
                            "Undefined variable '{}'.",
                            std::str::from_utf8_unchecked(
                                &name.value().chars[0..name.value().chars.count()]
                            )
                        );
                        return InterpretResult::RuntimeError;
                    }
                }
                i if i == OpCode::GetUpvalue as u8 => {
                    let slot = read_byte!();
                    self.push(
                        *(*frame).closure.value().upvalues[usize::from(slot)]
                            .unwrap()
                            .value()
                            .location,
                    );
                }
                i if i == OpCode::SetUpvalue as u8 => {
                    let slot = read_byte!();
                    *(*frame).closure.value().upvalues[usize::from(slot)]
                        .unwrap()
                        .value()
                        .location = self.peek(0);
                }
                i if i == OpCode::GetProperty as u8 => {
                    if !is_instance(self.peek(0)) {
                        runtime_error!(self, "Only instances have properties.");
                        return InterpretResult::RuntimeError;
                    }

                    let mut instance = as_instance(self.peek(0));
                    let name = read_string!();

                    if let Some(value) = instance.value_mut().fields.get(name) {
                        self.pop();
                        self.push(value);
                    } else if !self.bind_method(instance.value().class, name) {
                        return InterpretResult::RuntimeError;
                    }
                }
                i if i == OpCode::SetProperty as u8 => {
                    if !is_instance(self.peek(1)) {
                        runtime_error!(self, "Only instances have fields.");
                        return InterpretResult::RuntimeError;
                    }

                    let mut instance = as_instance(self.peek(1));
                    instance
                        .value_mut()
                        .fields
                        .set(read_string!(), self.peek(0));
                    let value = self.pop();
                    self.pop();
                    self.push(value);
                }
                i if i == OpCode::GetSuper as u8 => {
                    let name = read_string!();
                    let superclass = as_class(self.pop());

                    if !self.bind_method(superclass, name) {
                        return InterpretResult::RuntimeError;
                    }
                }
                i if i == OpCode::Index as u8 => {
                    if !is_list(self.peek(1)) {
                        runtime_error!(self, "Indexed value must be a list.");
                        return InterpretResult::RuntimeError;
                    }

                    if !self.peek(0).as_number().is_none() {
                        runtime_error!(self, "Index expression must be a number.");
                        return InterpretResult::RuntimeError;
                    }

                    let index = self.pop().as_number().unwrap();
                    let list = as_list(self.pop());

                    let val = list.value().items[index as usize];
                    self.push(val);
                }
                i if i == OpCode::Equal as u8 => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a == b).into());
                }
                i if i == OpCode::Greater as u8 => binary_op!(>),
                i if i == OpCode::Less as u8 => binary_op!(<),
                i if i == OpCode::Not as u8 => {
                    let shut_up_borrow_checker = self.pop();
                    self.push(shut_up_borrow_checker.is_falsey().into())
                }
                i if i == OpCode::Negate as u8 => {
                    if self.peek(0).as_number().is_none() {
                        runtime_error!(self, "Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                    let shut_up_borrow_checker = self.pop();
                    self.push((-shut_up_borrow_checker.as_number().unwrap()).into());
                }
                i if i == OpCode::Add as u8 => {
                    if is_string(self.peek(0)) && is_string(self.peek(1)) {
                        self.concatenate();
                    } else if self.peek(0).as_number().is_some()
                        && self.peek(1).as_number().is_some()
                    {
                        let b = self.pop().as_number().unwrap();
                        let a = self.pop().as_number().unwrap();
                        self.push((a + b).into());
                    } else {
                        runtime_error!(self, "Operands must be two numbers or two strings.");
                        return InterpretResult::RuntimeError;
                    }
                }
                i if i == OpCode::Subtract as u8 => binary_op!(-),
                i if i == OpCode::Multiply as u8 => binary_op!(*),
                i if i == OpCode::Divide as u8 => binary_op!(/),
                i if i == OpCode::Print as u8 => {
                    println!("{}", self.pop());
                }
                i if i == OpCode::Jump as u8 => {
                    let offset = read_short!();
                    (*frame).ip += usize::from(offset);
                }
                i if i == OpCode::JumpIfFalse as u8 => {
                    let offset = read_short!();
                    if self.peek(0).is_falsey() {
                        (*frame).ip += usize::from(offset);
                    }
                }
                i if i == OpCode::Loop as u8 => {
                    let offset = read_short!();
                    (*frame).ip -= usize::from(offset);
                }
                i if i == OpCode::Call as u8 => {
                    let arg_count = read_byte!();
                    let shut_up_borrow_checker = self.peek(arg_count.into());
                    if !self.call_value(shut_up_borrow_checker, arg_count.into()) {
                        return InterpretResult::RuntimeError;
                    }
                    frame = &mut self.frames[self.frame_count as usize - 1];
                }
                i if i == OpCode::Invoke as u8 => {
                    let method = read_string!();
                    let arg_count = read_byte!();
                    if !self.invoke(method, arg_count as i32) {
                        return InterpretResult::RuntimeError;
                    }
                    frame = &mut self.frames[self.frame_count as usize - 1];
                }
                i if i == OpCode::SuperInvoke as u8 => {
                    let method = read_string!();
                    let arg_count = read_byte!();
                    let superclass = as_class(self.pop());
                    if !self.invoke_from_class(superclass, method, arg_count as i32) {
                        return InterpretResult::RuntimeError;
                    }
                    frame = &mut self.frames[self.frame_count as usize - 1];
                }
                i if i == OpCode::Closure as u8 => {
                    let function = as_function(read_constant!());
                    let mut closure = new_closure(function);
                    self.push(closure.into());
                    for i in 0..closure.value().upvalues.count() {
                        let is_local = read_byte!();
                        let index = read_byte!();
                        if is_local == 1 {
                            closure.value_mut().upvalues[i] =
                                Some(self.capture_upvalue((*frame).slots.offset(index as isize)));
                        } else {
                            closure.value_mut().upvalues[i] =
                                (*frame).closure.value().upvalues[usize::from(index)];
                        }
                    }
                }
                i if i == OpCode::CloseUpvalue as u8 => {
                    self.close_upvalues(self.stack_top.sub(1));
                    self.pop();
                }
                i if i == OpCode::Return as u8 => {
                    let result = self.pop();
                    self.close_upvalues((*frame).slots);
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::Ok;
                    }
                    self.stack_top = (*frame).slots;
                    self.push(result);
                    frame = &mut self.frames[self.frame_count as usize - 1];
                }
                i if i == OpCode::Class as u8 => {
                    self.push(new_class(read_string!()).into());
                }
                i if i == OpCode::Inherit as u8 => {
                    let superclass = self.peek(1);
                    if !is_class(superclass) {
                        runtime_error!(self, "Superclass must be a class.");
                        return InterpretResult::RuntimeError;
                    }
                    let mut subclass = as_class(self.peek(0));
                    subclass
                        .value_mut()
                        .methods
                        .add_all(&mut as_class(superclass).value_mut().methods);
                    self.pop();
                }
                i if i == OpCode::Method as u8 => {
                    self.define_method(read_string!());
                }
                _ => break,
            }
        }

        InterpretResult::RuntimeError
    }

    pub unsafe fn interpret(&mut self, source: &str) -> InterpretResult {
        let function = compile(source);
        if function.is_none() {
            return InterpretResult::CompileError;
        }
        let function = function.unwrap();

        self.push(function.into());
        let closure = new_closure(function);
        self.pop();
        self.push(closure.into());
        self.call(closure, 0);

        let result = self.run();

        result
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        unsafe {
            self.globals.free();
            self.init_string = Ref::dangling();
            free_objects();
        }
    }
}
