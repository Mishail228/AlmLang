use std::cmp::Ordering;
use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
    Unary { op: UnaryOp, expr: Box<Expr> },
    Literal(Value),
    Variable(String),
    Call { name: String, args: Vec<Expr> },
    Grouping(Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    VarDecl { name: String, init: Option<Expr> },
    Assign { name: String, expr: Expr },
    ExprStmt(Expr),
    Return(Expr),
    If { cond: Expr, then: Block, else_branch: Option<Block> },
    While { cond: Expr , body: Block },
    Function { name: String, params: Vec<String>, body: Block },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
}

impl Value {
    #[inline]
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Char(c) => *c != '\0',
        }
    }
}

impl PartialEq<Self> for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (Value::Int(a), Value::Int(b)) => *a == *b,
            (Value::Int(a), Value::Float(b)) => *a as f64 == *b,
            (Value::Int(a), Value::Char(b)) => *a == *b as i64,
            (Value::Int(a), Value::Bool(b)) => Value::Int(*a).is_truthy() == *b,
            (Value::Float(a), Value::Float(b)) => *a == *b,
            (Value::Float(a), Value::Int(b)) => *a == *b as f64,
            (Value::Float(a), Value::Char(b)) => *a == *b as i64 as f64,            
            (Value::Float(a), Value::Bool(b)) => Value::Float(*a).is_truthy() == *b,
            (Value::Char(a), Value::Char(b)) => *a == *b,
            (Value::Char(a), Value::Int(b)) => *a as i64 == *b,
            (Value::Char(a), Value::Float(b)) => *a as i64 as f64 == *b,
            (Value::Char(a), Value::Bool(b)) => Value::Char(*a).is_truthy() == *b,
            (Value::Bool(a), Value::Bool(b)) => *a == *b,
            (Value::Bool(a), Value::Int(b)) => Value::Int(*b).is_truthy() == *a,
            (Value::Bool(a), Value::Float(b)) => Value::Float(*b).is_truthy() == *a,
            (Value::Bool(a), Value::Char(b)) => Value::Char(*b).is_truthy() == *a,
            (Value::Bool(a), Value::String(b)) => Value::String(b.clone()).is_truthy() == *a,
            (Value::String(a), Value::String(b)) => *a == *b,
            (Value::String(a), Value::Bool(b)) => Value::String(a.clone()).is_truthy() == *b,
            _ => false
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Int(a), Value::Float(b)) => (*a as f64).partial_cmp(b),
            (Value::Int(a), Value::Char(b)) => a.partial_cmp(&(*b as i64)),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Value::Float(a), Value::Char(b)) => a.partial_cmp(&(*b as i64 as f64)),
            (Value::Char(a), Value::Char(b)) => a.partial_cmp(b),
            (Value::Char(a), Value::Int(b)) => (*a as i64).partial_cmp(b),
            (Value::Char(a), Value::Float(b)) => (*a as i64 as f64).partial_cmp(b),
            _ => None
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    EqualEqual,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    And,
    Or,
    Xor,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    LShift,
    RShift,
}

impl From<Token> for BinaryOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Plus => BinaryOp::Plus,
            Token::Minus => BinaryOp::Minus,
            Token::Star => BinaryOp::Star,
            Token::Slash => BinaryOp::Slash,
            Token::DoubleEqual => BinaryOp::EqualEqual,
            Token::NotEqual => BinaryOp::NotEqual,
            Token::Greater => BinaryOp::Greater,
            Token::Less => BinaryOp::Less,
            Token::GreaterEqual => BinaryOp::GreaterEqual,
            Token::LessEqual => BinaryOp::LessEqual,
            Token::And => BinaryOp::And,
            Token::Or => BinaryOp::Or,
            Token::Xor => BinaryOp::Xor,
            Token::BitwiseAnd => BinaryOp::BitAnd,
            Token::BitwiseOr => BinaryOp::BitOr,
            Token::BitwiseXor => BinaryOp::BitXor,
            Token::BitwiseNot => BinaryOp::BitNot,
            _ => panic!("Unexpected token {:?}", token)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Minus,
    Not,
    BitNot
}

impl From<Token> for UnaryOp {
    fn from(token: Token) -> Self {
        match token {
            Token::Minus => UnaryOp::Minus,
            Token::Not => UnaryOp::Not,
            Token::BitwiseNot => UnaryOp::BitNot,
            _ => panic!("Unexpected token {:?}", token)
        }
    }
}