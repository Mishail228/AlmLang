use std::{collections::HashMap};

use crate::ast::{Type, Value};

#[derive(Debug, Clone)]
pub struct Function {
    pub arity: usize,
    pub locals_count: usize,
    pub ret_t: Type,
    pub name: String,
    pub chunk: Vec<OpCode>,
}

#[derive(Debug)]
struct CallFrame {
    ip: usize,
    base_ptr: usize,
    func: Function,
}

#[derive(Debug)]
pub struct VM {
    stack: Vec<Value>,
    functions: HashMap<String, Function>,
    frames: Vec<CallFrame>
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            functions: HashMap::new(),
            frames: Vec::new()
        }
    }

    pub fn add_function(&mut self, func: Function) {
        self.functions.insert(func.name.clone(), func);
    }

    pub fn run(&mut self, main_func_name: String) -> Result<Value, String> {
        let main_func = self.functions.get(&main_func_name)
            .cloned()
            .ok_or(format!("Function '{}' not found!", main_func_name))?;

        self.stack.clear();
        for _ in 0..main_func.locals_count {
            self.stack.push(Value::Int(0));
        }

        self.frames.push(CallFrame {
            ip: 0,
            func: main_func,
            base_ptr: 0,
        });

        self.execute()
    }
    fn execute(&mut self) -> Result<Value, String> {
        loop {
            if self.frames.is_empty() {
                return Ok(Value::Int(0));
            }

            let frame_idx = self.frames.len() - 1;
            let ip = self.frames[frame_idx].ip;

            if ip >= self.frames[frame_idx].func.chunk.len() {
                if self.frames.len() == 1 {
                    return Ok(Value::Int(1));
                } else {
                    self.handle_return(Value::Int(0))?;
                    continue;
                }
            }
            let instr = self.frames[frame_idx].func.chunk[ip].clone();
            self.frames[frame_idx].ip += 1;

            match instr {
                OpCode::Const(v) => {
                    self.stack.push(v.clone());
                },
                OpCode::Add => {
                    let b = self.stack.pop().ok_or("ADD: Stack underflow")?;
                    let a = self.stack.pop().ok_or("ADD: Stack underflow")?;
                    self.stack.push(match(a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
                        _ => return Err("ADD: Unsupported types".into()),
                    });
                },
                OpCode::Sub => {
                    let b = self.stack.pop().ok_or("SUB: Stack underflow")?;
                    let a = self.stack.pop().ok_or("SUB: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f64),
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 - b),
                        _ => return Err("SUB: Unsupported types".into()),
                    });
                },
                OpCode::Mul => {
                    let b = self.stack.pop().ok_or("MUL: Stack underflow")?;
                    let a = self.stack.pop().ok_or("MUL: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
                        (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
                        _ => return Err("MUL: Unsupported types".into()),
                    });
                },
                OpCode::Div => {
                    let b = self.stack.pop().ok_or("DIV: Stack underflow")?;
                    let a = self.stack.pop().ok_or("DIV: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => {
                            if b == 0 {
                                return Err("Division by zero".into());
                            }
                            Value::Int(a / b)
                        },
                        (Value::Float(a), Value::Float(b)) => {
                            if b == 0.0 {
                                return Err("Division by zero".into());
                            }
                            Value::Float(a / b)
                        },
                        (Value::Float(a), Value::Int(b)) => {
                            if b == 0 {
                                return Err("Division by zero".into());
                            }
                            Value::Float(a / b as f64)
                        },
                        (Value::Int(a), Value::Float(b)) => {
                            if b == 0.0 {
                                return Err("Division by zero".into());
                            }
                            Value::Float(a as f64 / b)
                        },
                        _ => return Err("DIV: Unsupported types".into()),
                    });
                },
                OpCode::Neg => {
                    let a = self.stack.pop().ok_or("NEG: Stack underflow")?;
                    self.stack.push(match a {
                        Value::Int(a) => Value::Int(-a),
                        Value::Float(a) => Value::Float(-a),
                        _ => return Err("NEG: Unsupported types".into()),
                    })
                },
                OpCode::Not => {
                    let a = self.stack.pop().ok_or("NOT: Stack underflow")?;
                    self.stack.push(Value::Bool(!a.is_truthy()));
                },
                OpCode::And => {
                    let b = self.stack.pop().ok_or("AND: Stack underflow")?;
                    let a = self.stack.pop().ok_or("AND: Stack underflow")?;
                    self.stack.push(Value::Bool(a.is_truthy() && b.is_truthy()));
                },
                OpCode::Or => {
                    let b = self.stack.pop().ok_or("OR: Stack underflow")?;
                    let a = self.stack.pop().ok_or("OR: Stack underflow")?;
                    self.stack.push(Value::Bool(a.is_truthy() || b.is_truthy()));
                },
                OpCode::Xor => {
                    let b = self.stack.pop().ok_or("XOR: Stack underflow")?;
                    let a = self.stack.pop().ok_or("XOR: Stack underflow")?;
                    self.stack.push(Value::Bool(a.is_truthy() ^ b.is_truthy()));
                }
                OpCode::Equal => {
                    let b = self.stack.pop().ok_or("EQ: Stack underflow")?;
                    let a = self.stack.pop().ok_or("EQ: Stack underflow")?;
                    self.stack.push(Value::Bool(a == b));
                },
                OpCode::NEqual => {
                    let b = self.stack.pop().ok_or("NEqual: Stack underflow")?;
                    let a = self.stack.pop().ok_or("NEqual: Stack underflow")?;
                    self.stack.push(Value::Bool(a != b));
                }
                OpCode::Less => {
                    let b = self.stack.pop().ok_or("LT: Stack underflow")?;
                    let a = self.stack.pop().ok_or("LT: Stack underflow")?;
                    self.stack.push(Value::Bool(a < b));
                },
                OpCode::Greater => {
                    let b = self.stack.pop().ok_or("GT: Stack underflow")?;
                    let a = self.stack.pop().ok_or("GT: Stack underflow")?;
                    self.stack.push(Value::Bool(a > b));
                },
                OpCode::LessEqual => {
                    let b = self.stack.pop().ok_or("LE: Stack underflow")?;
                    let a = self.stack.pop().ok_or("LE: Stack underflow")?;
                    self.stack.push(Value::Bool(a <= b));
                },
                OpCode::GreaterEqual => {
                    let b = self.stack.pop().ok_or("GE: Stack underflow")?;
                    let a = self.stack.pop().ok_or("GE: Stack underflow")?;
                    self.stack.push(Value::Bool(a >= b));
                },
                OpCode::BitNot => {
                    let a = self.stack.pop().ok_or("BITNOT: Stack underflow")?;
                    self.stack.push(match a {
                        Value::Int(a) => Value::Int(!a),
                        _ => return Err("BITNOT: Unsupported types".into()),
                    })
                },
                OpCode::BitAnd => {
                    let b = self.stack.pop().ok_or("BITAND: Stack underflow")?;
                    let a = self.stack.pop().ok_or("BITAND: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a & b),
                        _ => return Err("BITAND: Unsupported types".into()),
                    })
                },
                OpCode::BitOr => {
                    let b = self.stack.pop().ok_or("BITOR: Stack underflow")?;
                    let a = self.stack.pop().ok_or("BITOR: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a | b),
                        _ => return Err("BITOR: Unsupported types".into()),
                    })
                },
                OpCode::BitXor => {
                    let b = self.stack.pop().ok_or("BITXOR: Stack underflow")?;
                    let a = self.stack.pop().ok_or("BITXOR: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a ^ b),
                        _ => return Err("BITOR: Unsupported types".into()),
                    })
                },
                OpCode::LShift => {
                    let b = self.stack.pop().ok_or("LSHIFT: Stack underflow")?;
                    let a = self.stack.pop().ok_or("LSHIFT: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a << b),
                        _ => return Err("LSHIFT: Unsupported types".into()),
                    })
                },
                OpCode::RShift => {
                    let b = self.stack.pop().ok_or("RSHIFT: Stack underflow")?;
                    let a = self.stack.pop().ok_or("RSHIFT: Stack underflow")?;
                    self.stack.push(match (a, b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a >> b),
                        _ => return Err("RSHIFT: Unsupported types".into()),
                    })
                }
                OpCode::GetLocal(idx) => {
                    let frame = &self.frames[frame_idx];
                    if idx >= frame.func.locals_count {
                        return Err(format!("GETLOCAL: Local variable index {} out of bounds(max {})", 
                                        idx, frame.func.locals_count - 1));
                    }
                    let act_idx = frame.base_ptr + idx;
                    let v = self.stack[act_idx].clone();
                    self.stack.push(v);
                },
                OpCode::SetLocal(idx) => {
                    let val = self.stack.pop().ok_or("SETLOCAL: Stack underflow")?;
                    let frame = &self.frames[frame_idx];
                    if idx >= frame.func.locals_count {
                        return Err(format!("Local variable index {} out of bounds (max {})", 
                                         idx, frame.func.locals_count - 1));
                    }
                    let actual_idx = frame.base_ptr + idx;
                    self.stack[actual_idx] = val;
                },
                OpCode::Jump(target) => {
                    self.frames[frame_idx].ip = target;
                },
                OpCode::FJump(target) => {
                    let cond = self.stack.pop().ok_or("FJUMP: Stack underflow")?;
                    if !cond.is_truthy() {
                        self.frames[frame_idx].ip = target;
                    }
                },
                OpCode::TJump(target) => {
                    let cond = self.stack.pop().ok_or("TJUMP: Stack underflow")?;
                    if cond.is_truthy() {
                        self.frames[frame_idx].ip = target;
                    }
                },
                OpCode::Call(arity) => {
                    self.call_function(arity)?;
                },
                OpCode::Return => {
                    let res = self.stack.pop().unwrap_or(Value::Int(0));
                    if self.frames.len() <= 1 {
                        return Ok(res);
                    }
                    self.handle_return(res)?;
                },
                OpCode::Pop => {
                    self.stack.pop().ok_or("POP: Stack underflow")?;
                },
                OpCode::NoOp => {},
                OpCode::Halt => {
                    let res = self.stack.pop().unwrap_or(Value::Int(0));
                    return Ok(res);
                }
            }
        }
    }

    fn call_function(&mut self, arity: usize) -> Result<(), String> {
        let func_name = self.stack.pop().ok_or("CALL: No function name on stack")?;
        let func_name = match func_name {
            Value::String(name) => name,
            _ => return Err("CALL: Function name must be a string".to_string()),
        };

        if self.stack.len() < arity {
            return Err(format!("CALL: Not enough arguments for function '{}'", func_name));
        }

        let func = self.functions.get(&func_name)
            .cloned()
            .ok_or(format!("CALL: Function '{}' not found", func_name))?;

        if func.arity != arity {
            return Err(format!("CALL: Function '{}' expects {} arguments, but {} were provided", func_name, func.arity, arity));
        }

        let cur_stack_size = self.stack.len();
        let base_ptr = cur_stack_size - arity;

        for _ in arity..func.locals_count {
            self.stack.push(Value::Int(0));
        }

        let new_frame = CallFrame {
            ip: 0,
            func,
            base_ptr,
        };
        self.frames.push(new_frame);
        Ok(())
    }

    fn handle_return(&mut self, val: Value) -> Result<(), String> {
        if self.frames.len() <= 1 {
            return Ok(());
        }

        let finished_frame = self.frames.pop().ok_or("RET: No frame to return from")?;
        self.stack.truncate(finished_frame.base_ptr);
        self.stack.push(val);

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum OpCode {
    Const(Value), // Положить значение на стек,

    Add, // Сложить два верхних значения стека
    Sub, // Вычесть два верхних значения со стека
    Mul, // Умножить два верхних значения со стека
    Div, // Поделить два верхних значения со стека
    Neg, // Применить унарный минус к верхнему значению со стека

    Equal, // Сравнить равенство двух верхних значения стека
    NEqual, // Сравнить не равенство двух верхних значения стека
    Greater, // Сравнить больше ли верхнее значение предверхнего
    GreaterEqual, // Сравнить больше или равно верхнее значение предверхнего
    Less, // Сравнить меньше ли верхнее значение предверхнего
    LessEqual, // Сравнить меньше или равно верхнее значение предверхнего

    Not, // Логическое отрицание верхнего значения стека
    And, // Логическое И двух верхних значений стека
    Or, // Логическое ИЛИ двух верхних значений стека
    Xor,

    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    GetLocal(usize), // Загрузить значение локальной переменной на верх стека
    SetLocal(usize), // Выгрузить значение с верха стека в локальную переменную
    Jump(usize), // Безусловный переход
    FJump(usize), // Переход, если сверху стека значение False
    TJump(usize),
    Return, // Завершить выполнение текущей функции
    Call(usize), // Вызвать функцию

    Pop,
    NoOp,
    Halt, // Глобальный Return
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_functions() {
        let mut vm = VM::new();
        let add_func = Function {
            name: "add".to_string(),
            chunk: vec![
                OpCode::GetLocal(0),
                OpCode::GetLocal(1),
                OpCode::Add,
                OpCode::Return,
            ],
            ret_t: Type::Int,
            arity: 2,
            locals_count: 2,
        };

        vm.add_function(add_func);

        let main_func = Function {
            name: "main".to_string(),
            chunk: vec![
                OpCode::Const(Value::Int(5)),
                OpCode::Const(Value::Int(3)),
                OpCode::Const(Value::String("add".to_string())),
                OpCode::Call(2),
                OpCode::Halt
            ],
            ret_t: Type::Void,
            arity: 0,
            locals_count: 0,
        };

        vm.add_function(main_func);

        let res = vm.run("main".to_string()).unwrap();
        println!("Result: {:?}", res);
    }
    #[test]
    fn test_function_locals() {
        let mut vm = VM::new();
        let add_func = Function {
            name: "calc".to_string(),
            chunk: vec![
                OpCode::GetLocal(0),
                OpCode::GetLocal(1),
                OpCode::Mul,
                OpCode::SetLocal(3),
                
                OpCode::GetLocal(3),
                OpCode::GetLocal(2),
                OpCode::Add,
                OpCode::Return,
            ],
            ret_t: Type::Int,
            arity: 3,
            locals_count: 4,
        };

        vm.add_function(add_func);

        let main_func = Function {
            name: "main".to_string(),
            chunk: vec![
                OpCode::Const(Value::Int(5)),
                OpCode::Const(Value::Int(3)),
                OpCode::Const(Value::Int(10)),
                OpCode::Const(Value::String("calc".to_string())),
                OpCode::Call(3),
                OpCode::Halt
            ],
            ret_t: Type::Void,
            arity: 0,
            locals_count: 0,
        };

        vm.add_function(main_func);

        let res = vm.run("main".to_string()).unwrap();
        println!("Result: {:?}", res);
    }
}