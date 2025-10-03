use crate::ast::{BinaryOp, Block, Expr, Stmt, Type, UnaryOp, Value};
use std::collections::HashMap;

pub struct TypeChecker {
    vars: HashMap<String, Type>, // Name, Type
    funcs: HashMap<String, (Vec<(String, Type)>, Type)>, // Name, params, return_type
    cur_func_ret_t: Option<Type>, 
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            funcs: HashMap::new(),
            cur_func_ret_t: None,
        }
    }
    pub fn check(&mut self, code: &mut Vec<Stmt>) -> Result<(), String> {
        self.collect_functions(code)?;

        for stmt in code {
            self.check_stmt(stmt)?;
        }

        Ok(())
    }
    fn collect_functions(&mut self, stmts: &Vec<Stmt>) -> Result<(), String> {
        for stmt in stmts {
            if let Stmt::Function { name, params, return_type, .. } = stmt {
                let param_types = params.iter().map(|(name, t)| (name.clone(), t.clone())).collect();
                self.funcs.insert(name.clone(), (param_types, return_type.clone()));
            }
        }
        Ok(())
    }
    fn check_stmt(&mut self, stmt: &mut Stmt) -> Result<(), String> {
        match stmt {
            Stmt::VarDecl { name, init, var_type } => self.check_var_decl(name, init, var_type)?,
            Stmt::Assign { name, expr, ..} => self.check_assign(name, expr)?,
            Stmt::Function { name, params, return_type, body } => self.check_function(name, params, return_type, body)?,
            _ => {}
        };
        Ok(())
    }
    fn check_var_decl(&mut self, name: &str, init: &mut Option<Expr>, var_type: &mut Type) -> Result<(), String> {
        if let Some(init_expr) = init {
            let init_type = self.check_expr_and_upd_t(init_expr)?;
            if *var_type == Type::Unknown {
                *var_type = init_type;
            }
            if !self.types_compatible(&var_type, &init_type) {
                return Err(format!("Can't assign {} to variable {} of type {}", init_type, name, var_type));
            }
        }
        self.vars.insert(name.to_string(), var_type.clone());
        Ok(())
    }
    fn check_assign(&mut self, name: &str, expr: &mut Expr) -> Result<(), String> {
        let var_t = self.vars.get(name)
            .cloned()
            .ok_or(format!("Can't assign to undefined variable '{}'", name))?;

        let expr_t = self.check_expr_and_upd_t(expr)?;

        if !self.types_compatible(&var_t, &expr_t) {
            return Err(format!("Can't assign {} to variable '{}' of type {}", expr_t, name, var_t));
        }
        Ok(())
    }
    fn check_function(&mut self, name: &str, params: &Vec<(String, Type)>, return_type: &mut Type, body: &mut Block) -> Result<(), String> {
        let old_vars = self.vars.clone();
        let old_func_ret_t = self.cur_func_ret_t.take();

        self.cur_func_ret_t = Some(*return_type);
        for (p_name, p_type) in params {
            self.vars.insert(p_name.clone(), *p_type);
        }

        let has_ret = self.check_function_body(name, &mut body.stmts, return_type)?;

        if *return_type != Type::Void && !has_ret {
            return Err(format!("No return in function {}, but it has non-void return type", name));
        }

        self.vars = old_vars;
        self.cur_func_ret_t = old_func_ret_t;

        Ok(())
    }
    fn check_function_body(&mut self, name: &str, stmts: &mut [Stmt], exp_ret_t: &Type) -> Result<bool, String> {
        let mut has_explicit_return = false;

        for stmt in stmts {
            match stmt {
                Stmt::Return(expr) => {
                    let act_ret_t = self. check_expr_and_upd_t(expr)?;
                    if !self.types_compatible(exp_ret_t, &act_ret_t) {
                        return Err(format!("Function {} expects return type {}, but it returns {}", name, exp_ret_t, act_ret_t)); // Err
                    }

                    has_explicit_return = true;
                }
                Stmt::If {then, else_branch, .. } => {
                    let then_has_ret = self.check_function_body(name, &mut then.stmts, exp_ret_t)?;

                    if let Some(else_block) = else_branch {
                        let else_has_ret = self.check_function_body(name, &mut else_block.stmts, exp_ret_t)?;

                        if then_has_ret && else_has_ret {
                            has_explicit_return = true;
                        }
                    }
                }
                Stmt::While { cond, body } => {
                    let cond_t = self. check_expr_and_upd_t(cond)?;
                    if cond_t != Type::Bool {
                        return Err(format!("While condition must be boolean, but got {}", cond_t));
                    }
                    let _ = self.check_function_body(name, &mut body.stmts, exp_ret_t)?;
                }
                _ => {
                    self.check_stmt(stmt)?;
                }
            }
        }
        Ok(has_explicit_return)
    }
    #[allow(unreachable_patterns)]
    fn check_expr_and_upd_t(&mut self, expr: &mut Expr) -> Result<Type, String> {
        let inferred_t = match expr {
            Expr::Literal { val } => {
                match val {
                    Value::Int(_) => Type::Int,
                    Value::Float(_) => Type::Float,
                    Value::Bool(_) => Type::Bool,
                    Value::Char(_) => Type::Char,
                    Value::String(_) => Type::String,
                }
            },
            Expr::Variable { name, .. } => {
                self.vars.get(name)
                    .cloned()
                    .ok_or(format!("Undefined variable '{}'", name))?
            },
            Expr::Binary { op, left, right, ..} => {
                let left_t = self.check_expr_and_upd_t(left)?;
                let right_t = self.check_expr_and_upd_t(right)?;

                self.check_binary_op(op, &left_t, &right_t)?
            },
            Expr::Unary { op, expr, .. } => {
                let inner_t = self.check_expr_and_upd_t(expr)?;

                self.check_unary_op(op, &inner_t)?
            },
            Expr::Call { name, args, .. } => self.check_function_call(name, args)?,
            _ => Type::Unknown,
        };

        if let Some(field) = expr.get_type_field_mut() {
            *field = inferred_t;
        }
        Ok(inferred_t)
    }
    #[allow(unreachable_patterns)]
    fn check_binary_op(&self, op: &BinaryOp, left_t: &Type, right_t: &Type) -> Result<Type, String> {
        match op {
            BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Star | BinaryOp::Slash => {
                match (&left_t, &right_t) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Float, Type::Float) => Ok(Type::Float),
                    (Type::Int, Type::Float) | (Type::Float, Type::Int) => Ok(Type::Float),
                    _ => Err(format!("Can't perform arithmetic operation on types {} and {}", left_t, right_t))
                }
            },
            BinaryOp::EqualEqual | BinaryOp::NotEqual | BinaryOp::Greater | BinaryOp::Less | BinaryOp::GreaterEqual | BinaryOp::LessEqual => {
                if self.types_comparable(&left_t, &right_t) {
                    Ok(Type::Bool)
                } else {
                    Err(format!("Can't compare types {} and {}", left_t, right_t))
                }
            }
            BinaryOp::And | BinaryOp::Or | BinaryOp::Xor => {
                if left_t == &Type::Bool && right_t == &Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(format!("Logical operations require boolean operands, got {} and {}", left_t, right_t))
                }
            }
            BinaryOp::BitAnd | BinaryOp::BitNot | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::LShift | BinaryOp::RShift => {
                if left_t == &Type::Int && right_t == &Type::Int {
                    Ok(Type::Int)
                } else {
                    Err(format!("Bitwise operations works only with integer operands, got {} and {}", left_t, right_t))
                }
            }
            _ => unimplemented!("Type checker doesn't support this binary operation: {:?}", op)
        }
    }
    #[allow(unreachable_patterns)]
    fn check_unary_op(&self, op: &UnaryOp, expr_t: &Type) -> Result<Type, String> {
        match op {
            UnaryOp::Minus => {
                if expr_t == &Type::Int || expr_t == &Type::Float {
                    Ok(*expr_t)
                } else {
                    Err(format!("Unary minus requires numeric type, got {}", expr_t))
                }
            }
            UnaryOp::BitNot => {
                if expr_t == &Type::Int {
                    Ok(*expr_t)
                } else {
                    Err(format!("Bitwise not requires integer type, got {}", expr_t))
                }
            }
            UnaryOp::Not => {
                if expr_t == &Type::Bool {
                    Ok(*expr_t)
                } else {
                    Err(format!("Logic not requires boolean type, got {}", expr_t))
                }
            }
            _ => unimplemented!("Type checker doesn't support this unary operation: {:?}", op)
        }
    }
    fn check_function_call(&mut self, name: &str, args: &mut Vec<Expr>) -> Result<Type, String> {
        let (params_t, ret_t) = self.funcs.get(name)
            .cloned()
            .ok_or(format!("Function '{}' not found", name))?;

        if args.len() != params_t.len() {
            return Err(format!("Function '{}' expects {} arguments, but {} were provided", name, params_t.len(), args.len()));
        }

        for (i, (arg, exp_t)) in args.iter_mut().zip(params_t.iter()).enumerate() {
            let act_t = self.check_expr_and_upd_t(arg)?;
            if !self.types_compatible(&exp_t.1, &act_t) {
                return Err(format!("Argument {} of function '{}' expects {}, but got {}", i + 1, name, exp_t.1, act_t));
            }
        }

        Ok(ret_t)
    }
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool { // Are implicit conversions allowed for expected and actual types
        if expected == actual {
            return true;
        }
        match (expected, actual) {
            (Type::Float, Type::Int) => true,
            _ => false
        }
    }
    fn types_comparable(&self, left: &Type, right: &Type) -> bool {
        left == right 
        || (left == &Type::Int && right == &Type::Float) 
        || (left == &Type::Float && right == &Type::Int)
    }
}