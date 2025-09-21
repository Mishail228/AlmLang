use std::collections::HashMap;

use crate::ast::*;
use crate::vm::{OpCode, Function};

#[derive(Debug)]
pub struct Compiler {
    functions: Vec<Function>,
    cur_chunk: Vec<OpCode>,
    cur_func_name: String,
    locals_count: usize,
    local_names: HashMap<String, usize> 
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            cur_chunk: Vec::new(),
            cur_func_name: String::new(),
            locals_count: 0,
            local_names: HashMap::new(),
        }
    }
    pub fn compile_ast(&mut self, statements: Vec<Stmt>) -> Result<Vec<Function>, String> {
        self.functions.clear();
        self.cur_chunk.clear();
        self.cur_func_name.clear();
        self.locals_count = 0;
        self.local_names.clear();

        for stmt in statements {
            if let Stmt::Function { name, params, body } = stmt {
                self.compile_function(name, params, body)?;
            } else {
                return Err("All statements must be inside functions. Use 'fn main() { ... }' for entry point.".to_string());
            }
        }
        if !self.functions.iter().any(|f| f.name == "main") {
            return Err("No 'main' function found. Entry point required.".to_string());
        }

        Ok(self.functions.clone())
    }
    fn compile_stmt(&mut self, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::VarDecl { name, init } => self.compile_var_decl(name, init)?,
            Stmt::Assign { name, expr } => self.compile_assign(name, expr)?,
            Stmt::ExprStmt(expr) => {
                self.compile_expr(expr)?;
                self.emit(OpCode::Pop);
            },
            Stmt::Return(expr) => self.compile_return(expr)?,
            Stmt::If { cond, then, else_branch } => self.compile_if(cond, then, else_branch)?,
            Stmt::While { cond, body } => self.compile_while(cond, body)?,
            Stmt::Function { name, params, body } => self.compile_function(name, params, body)?,
        }
        Ok(())
    }
    fn compile_expr(&mut self, expr: Expr) -> Result<(), String> {
        match expr {
            Expr::Literal(val) => self.emit(OpCode::Const(val)),
            Expr::Variable(name) => self.compile_variable(name)?,
            Expr::Binary { op, left, right } => self.compile_binary_op(*left, op, *right)?,
            Expr::Unary { op, expr } => self.compile_unary(op, *expr)?,
            Expr::Call { name, args } => self.compile_call(name, args)?,
            Expr::Grouping(expr) => self.compile_expr(*expr)?,
        }
        Ok(())
    }
    fn compile_var_decl(&mut self, name: String, init: Option<Expr>) -> Result<(), String> {
        let var_idx = self.locals_count;
        self.local_names.insert(name.clone(), var_idx);
        self.locals_count += 1;

        if let Some(init_expr) = init {
            self.compile_expr(init_expr)?;
            self.emit(OpCode::SetLocal(var_idx));
        } else {
            self.emit(OpCode::Const(Value::Int(0)));
            self.emit(OpCode::SetLocal(var_idx));
        }
        Ok(())
    }
    fn compile_assign(&mut self, name: String, expr: Expr) -> Result<(), String> {
        self.compile_expr(expr)?;

        let var_idx = *self.local_names.get(&name).ok_or(format!("Undefinded variable: {}", name))?;
        self.emit(OpCode::SetLocal(var_idx));

        Ok(())
    }
    fn compile_variable(&mut self, name: String) -> Result<(), String> {
        let var_idx = *self.local_names.get(&name).ok_or(format!("Undefined variable: {}", name))?;
        self.emit(OpCode::GetLocal(var_idx));
        Ok(())
    }
    fn compile_binary_op(&mut self, lhs: Expr, op: BinaryOp, rhs: Expr) -> Result<(), String> {
        self.compile_expr(lhs)?;
        self.compile_expr(rhs)?;

        let opcode = match op {
            BinaryOp::Plus => OpCode::Add,
            BinaryOp::Minus => OpCode::Sub,
            BinaryOp::Star => OpCode::Mul,
            BinaryOp::Slash => OpCode::Div,
            BinaryOp::EqualEqual => OpCode::Equal,
            BinaryOp::NotEqual => OpCode::NEqual,
            BinaryOp::Greater => OpCode::Greater,
            BinaryOp::GreaterEqual => OpCode::GreaterEqual,
            BinaryOp::Less => OpCode::Less,
            BinaryOp::LessEqual => OpCode::LessEqual,
            BinaryOp::And => OpCode::And,
            BinaryOp::Or => OpCode::Or,
            BinaryOp::Xor => OpCode::Xor,
            BinaryOp::BitAnd => OpCode::BitAnd,
            BinaryOp::BitOr => OpCode::BitOr,
            BinaryOp::BitXor => OpCode::BitXor,
            BinaryOp::BitNot => OpCode::BitNot,
            BinaryOp::LShift => OpCode::LShift,
            BinaryOp::RShift => OpCode::RShift,
            // TODO: LSHIFT, RSHIFT
        };

        self.emit(opcode);
        Ok(())
    }
    fn compile_unary(&mut self, op: UnaryOp, expr: Expr) -> Result<(), String> {
        self.compile_expr(expr)?;
        let opcode = match op {
            UnaryOp::Minus => OpCode::Neg,
            UnaryOp::Not => OpCode::Not,
            UnaryOp::BitNot => OpCode::BitNot,
        };
        self.emit(opcode);
        Ok(())
    }
    fn compile_call(&mut self, name: String, args: Vec<Expr>) -> Result<(), String> {
        let args_num = args.len();
        for arg in args {
            self.compile_expr(arg)?;
        }
        self.emit(OpCode::Const(Value::String(name)));
        self.emit(OpCode::Call(args_num));

        Ok(())
    }
    fn compile_return(&mut self, expr: Expr) -> Result<(), String> {
        self.compile_expr(expr)?;
        self.emit(OpCode::Return);
        Ok(())
    }
    fn compile_if(&mut self, cond: Expr, then_branch: Block, else_branch: Option<Block>) -> Result<(), String> {
        self.compile_expr(cond)?;

        let then_jump_pos = self.cur_chunk.len();
        self.emit(OpCode::FJump(0));

        for stmt in then_branch.stmts {
            self.compile_stmt(stmt)?;
        }

        if let Some(else_block) = else_branch {
            let else_jump_pos = self.cur_chunk.len();
            self.emit(OpCode::FJump(0));

            let else_start = self.cur_chunk.len();
            self.patch_jump(then_jump_pos, else_start);

            for stmt in else_block.stmts {
                self.compile_stmt(stmt)?;
            }

            let end_pos = self.cur_chunk.len();
            self.patch_jump(else_jump_pos, end_pos);
        } else {
            let end_pos = self.cur_chunk.len();
            self.patch_jump(then_jump_pos, end_pos);
        }

        Ok(())
    }
    fn compile_while(&mut self, cond: Expr, body: Block) -> Result<(), String> {
        let cond_start = self.cur_chunk.len();
        self.compile_expr(cond)?;

        let exit_jump_pos = self.cur_chunk.len();
        self.emit(OpCode::FJump(0));

        for stmt in body.stmts {
            self.compile_stmt(stmt)?;
        }

        self.emit(OpCode::Jump(cond_start));

        let end_pos = self.cur_chunk.len();
        self.patch_jump(exit_jump_pos, end_pos);
        Ok(())
    }
    fn compile_function(&mut self, name: String, params: Vec<String>, body: Block) -> Result<(), String> {
        let saved_chunk = std::mem::replace(&mut self.cur_chunk, Vec::new());
        let saved_locals_count = self.locals_count;
        let saved_local_names = std::mem::replace(&mut self.local_names, HashMap::new());
        let saved_function_name = std::mem::replace(&mut self.cur_func_name, name.clone());
        self.locals_count = 0;

        for (i, param) in params.iter().enumerate() {
            self.local_names.insert(param.clone(), i);
            self.locals_count += 1;
        }

        for stmt in body.stmts {
            self.compile_stmt(stmt)?;
        }

        if self.cur_chunk.is_empty() || !matches!(self.cur_chunk.last(), Some(OpCode::Return)) {
            self.emit(OpCode::Const(Value::Int(0)));
            self.emit(OpCode::Return);
        }

        let function = Function {
            name: name.clone(),
            chunk: self.cur_chunk.clone(),
            arity: params.len(),
            locals_count: self.locals_count
        };

        self.functions.push(function);
        
        self.cur_chunk = saved_chunk;
        self.locals_count = saved_locals_count;
        self.local_names = saved_local_names;
        self.cur_func_name = saved_function_name;

        Ok(())
    }

    fn emit(&mut self, opcode: OpCode) {
        self.cur_chunk.push(opcode);
    }
    fn patch_jump(&mut self, jump_pos: usize, target: usize) {
        match &mut self.cur_chunk[jump_pos] {
            OpCode::FJump(addr) => *addr = target,
            OpCode::TJump(addr) => *addr = target,
            OpCode::Jump(addr) => *addr = target,
            _ => panic!("Can't patch non-jump instruction"), // ? Why panic
        }
    }
}