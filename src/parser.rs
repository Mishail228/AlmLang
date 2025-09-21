#![allow(dead_code)]

use crate::ast::*;
use crate::lexer::Token;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    idx: usize,
}

impl Parser {
    #[inline]
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, idx: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while !self.at_end() {
            stmts.push(self.parse_stmt()?);
        }

        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(Token::VarDecl)? {
            self.parse_var_decl()
        } else if self.match_token(Token::Identifier("".to_string()))? {
            self.parse_assign()
        } else if self.match_token(Token::Return)? {
            self.parse_return()
        } else if self.match_token(Token::If)? {
            self.parse_if()
        } else if self.match_token(Token::While)? {
            self.parse_while()
        } else if self.match_token(Token::Function)? {
            self.parse_function()
        } else {
            self.parse_expr_stmt()
        }
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, ParseError> {
        let name: String;
        if let Some(t) = self.curr() {
            name = match t {
                Token::Identifier(n) => n .clone(),
                _ => return Err(ParseError::UnexpectedToken(t.clone()))
            };
        } else {
            return Err(ParseError::IndexOutOfBounds)
        }
        self.advance()?;

        if let Some(t) = self.curr() {
            match t {
                Token::Equal => {
                    self.advance()?;
                    let value = self.parse_expr()?;
                    self.expect_token(Token::Semicolon)?;
                    Ok(Stmt::VarDecl {
                        name,
                        init: Some(value)
                    })
                },
                Token::Semicolon => {
                    self.advance()?;
                    Ok(Stmt::VarDecl { name, init: None })
                },
                tok => Err(ParseError::ExpectedToken(Token::Semicolon, tok.clone()))
            }
        } else {
            Err(ParseError::IndexOutOfBounds)
        }
    }

    fn parse_assign(&mut self) -> Result<Stmt, ParseError> {
        let name: String;

        if let Some(t) = self.previous() {
            match t {
                Token::Identifier(n) => {
                    name = n.clone();
                },
                _ => unreachable!()
            }
        } else {
            return Err(ParseError::UsizeUnderflow);
        }

        self.expect_token(Token::Equal)?;
        let value = self.parse_expr()?;
        self.expect_token(Token::Semicolon)?;

        Ok(Stmt::Assign { name, expr: value })
    }

    fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::Return(expr))
    }

    fn parse_if(&mut self) -> Result<Stmt, ParseError> {
        self.expect_token(Token::LParen)?;
        let cond = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        let if_stmts = self.parse_block()?;

        let mut else_stmts: Block = Block { stmts: Vec::new() };

        if self.check_token(&Token::Else) {
            self.advance()?;
            if self.match_token(Token::If)? {
                else_stmts.stmts.push(self.parse_if()?);
            } else {
                else_stmts = self.parse_block()?;
            }
        }

        Ok(
            Stmt::If { 
                cond, 
                then: if_stmts, 
                else_branch:  if else_stmts.stmts.is_empty() {
                    None
                } else {
                    Some(else_stmts)
                }
            }
        )
    }

    fn parse_while(&mut self) -> Result<Stmt, ParseError> {
        self.expect_token(Token::LParen)?;
        let cond = self.parse_expr()?;
        self.expect_token(Token::RParen)?;
        let body = self.parse_block()?;

        Ok(
            Stmt::While { 
                cond, 
                body
            }
        )
    }

    fn parse_function(&mut self) -> Result<Stmt, ParseError> {
        let name = match self.curr() {
            Some(Token::Identifier(n)) => n.clone(),
            Some(t) => return Err(ParseError::UnexpectedToken(t.clone())),
            None => return Err(ParseError::IndexOutOfBounds),
        };
        self.advance()?;
        self.expect_token(Token::LParen)?;
        let mut params = Vec::new();
        if !self.check_token(&Token::RParen) {
            loop {
                let param;
                match self.curr() {
                    Some(Token::Identifier(n)) => {
                        param = n.clone();
                    }
                    Some(t) => return Err(ParseError::UnexpectedToken(t.clone())),
                    None => return Err(ParseError::IndexOutOfBounds),
                }
                self.advance()?;
                params.push(param);
                if self.check_token(&Token::RParen) {
                    break;
                }
                self.expect_token(Token::Comma)?;
            }
        }
        self.expect_token(Token::RParen)?;
        let body = self.parse_block()?;
        Ok(Stmt::Function { name, params, body })
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect_token(Token::LBrace)?;
        let mut stmts = Vec::new();
        while !self.check_token(&Token::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        self.expect_token(Token::RBrace)?;
        Ok(Block { stmts })
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expr()?;
        self.expect_token(Token::Semicolon)?;
        Ok(Stmt::ExprStmt(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_logic_or()
    }

    fn parse_logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logic_and()?;
        
        while self.match_token(Token::Or)? {
            let op = BinaryOp::Or;
            let right = self.parse_logic_and()?;
            
            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;

        while self.match_token(Token::And)? {
            let op = BinaryOp::And;
            let right = self.parse_equality()?;

            expr = Expr::Binary {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_comparison()?;

        while self.match_any(&[Token::DoubleEqual, Token::NotEqual])? {
            let op = self.previous().ok_or(ParseError::UsizeUnderflow)?.clone();
            let right = self.parse_comparison()?;

            expr = Expr::Binary {
                op: BinaryOp::from(op),
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_term()?;

        while self.match_any(&[Token::Greater, Token::GreaterEqual, Token::Less, Token::LessEqual])? {
            let op = self.previous().ok_or(ParseError::UsizeUnderflow)?.clone();
            let right = self.parse_term()?;
            
            expr = Expr::Binary {
                op: BinaryOp::from(op),
                left: Box::new(expr),
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_factor()?;

        while self.match_any(&[Token::Plus, Token::Minus])? {
            let op = self.previous().ok_or(ParseError::UsizeUnderflow)?.clone();
            let right = self.parse_factor()?;
            
            expr = Expr::Binary { 
                op: BinaryOp::from(op), 
                left: Box::new(expr), 
                right: Box::new(right) 
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;

        while self.match_any(&[Token::Slash, Token::Star])? {
            let op = self.previous().ok_or(ParseError::UsizeUnderflow)?.clone();
            let right = self.parse_unary()?;

            expr = Expr::Binary { 
                op: BinaryOp::from(op), 
                left: Box::new(expr), 
                right: Box::new(right) 
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_any(&[Token::Minus, Token::Not, Token::BitwiseNot])? {
            let op = match self.previous() {
                Some(t) => t.clone(),
                None => return Err(ParseError::UsizeUnderflow)
            };
            let right = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::from(op),
                expr: Box::new(right)
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let token = match self.curr() {
            Some(t) => t.clone(),
            None => return Err(ParseError::IndexOutOfBounds)
        };
        match token {
            Token::Int(n) => {
                self.advance()?;
                Ok(Expr::Literal(Value::Int(n)))
            },
            Token::Float(n) => {
                self.advance()?;
                Ok(Expr::Literal(Value::Float(n)))
            },
            Token::Char(c) => {
                self.advance()?;
                Ok(Expr::Literal(Value::Char(c)))
            },
            Token::String(s) => {
                self.advance()?;
                Ok(Expr::Literal(Value::String(s)))
            },
            Token::True => {
                self.advance()?;
                Ok(Expr::Literal(Value::Bool(true)))
            },
            Token::False => {
                self.advance()?;
                Ok(Expr::Literal(Value::Bool(false)))
            },
            Token::Identifier(name) => {
                self.advance()?;

                if self.match_token(Token::LParen)? {
                    let mut args = Vec::new();
                    if !self.check_token(&Token::RParen) {
                        loop {
                            args.push(self.parse_expr()?);
                            if !self.match_token(Token::Comma)? {
                                break;
                            }
                        }
                    }
                    self.expect_token(Token::RParen)?;
                    Ok(Expr::Call { name, args })
                } else {
                    Ok(Expr::Variable(name))
                }
            },
            Token::LParen => {
                self.advance()?;
                let expr = self.parse_expr()?;
                self.expect_token(Token::RParen)?;
                Ok(expr)
            }
            tok => Err(ParseError::UnexpectedToken(tok))
        }
    }

    fn expect_token(&mut self, token: Token) -> Result< (), ParseError> {
        if self.check_token(&token) {
            self.advance()?;
            return Ok(());
        }
        Err(ParseError::ExpectedToken(token, match self.curr() {
            Some(t) => t.clone(),
            None => return Err(ParseError::IndexOutOfBounds)
        }))
    }
    fn match_token(&mut self, token: Token) -> Result<bool, ParseError> {
        if self.check_token(&token) {
            self.advance()?;
            return Ok(true);
        }
        Ok(false)
    }
    fn check_token(&self, token: &Token) -> bool {
        let curr_token = match self.curr() {
            Some(t) => t,
            None => return false
        };
        match (curr_token, token) {
            (Token::Identifier(_), Token::Identifier(_)) => true,
            (Token::Int(_), Token::Int(_)) => true,
            (Token::Float(_), Token::Float(_)) => true,
            (Token::String(_), Token::String(_)) => true,
            (Token::Char(_), Token::Char(_)) => true,
            (a, b) => a == b,
        }
    }
    #[inline]
    fn at_end(&self) -> bool {
        self.idx >= self.tokens.len() || self.tokens[self.idx] == Token::EOF
    }
    #[inline]
    fn advance(&mut self) -> Result<(), ParseError> {
        if !self.at_end() {
            self.idx += 1;
            return Ok(());
        }
        Err(ParseError::IndexOutOfBounds)
    }
    #[cold]
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.idx + 1)
    }
    #[inline]
    fn curr(&self) -> Option<&Token> {
        self.tokens.get(self.idx)
    }
    #[cold]
    fn previous(&self) -> Option<&Token> {
        if self.idx == 0 {
            return None;
        }
        self.tokens.get(self.idx - 1)
    }
    fn match_any(&mut self, token_types: &[Token]) -> Result<bool, ParseError> {
        for token in token_types {
            let curr = match self.curr() {
                Some(t) => t.clone(),
                None => return Err(ParseError::IndexOutOfBounds)
            };
            if curr == *token {
                self.advance()?;
                return Ok(true);
            }
        }
        Ok(false)
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token),
    ExpectedToken(Token, Token), // Expected, Given
    ExpectedSomeToken,
    UsizeUnderflow,
    IndexOutOfBounds,
}

#[cfg(test)]
mod tests {
    use crate::ast::Stmt::ExprStmt;
    use crate::lexer::lex;
    use super::*;

    #[test]
    fn test_parse_expr() {
        let code = "10 + 11 * 12;";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            ExprStmt(
                Expr::Binary {
                    op: BinaryOp::Plus,
                    left: Box::new(Expr::Literal(Value::Int(10))),
                    right: Box::new(Expr::Binary {
                        op: BinaryOp::Star,
                        left: Box::new(Expr::Literal(Value::Int(11))),
                        right: Box::new(Expr::Literal(Value::Int(12))),
                    }),
                }
            )
        ];
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_var_decl() {
        let code = "let x;";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::VarDecl { name: String::from("x"), init: None },
        ];
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_var_decl_init() {
        let code = "let x = 10;";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::VarDecl {
                name: "x".to_string(), 
                init: Some(Expr::Literal(Value::Int(10)))
            }
        ];
        
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_assign() {
        let code = "x = 42;";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![Stmt::Assign { 
            name: String::from("x"), 
            expr: Expr::Literal(Value::Int(42)) 
        }];
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_return() {
        let code = "return 42 + 51;";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::Return(
                Expr::Binary {
                    op: BinaryOp::Plus,
                    left: Box::new(Expr::Literal(Value::Int(42))),
                    right: Box::new(Expr::Literal(Value::Int(51)))
                }
            )
        ];
        
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_if() {
        let code = "if(a==10){\
        let x = 25;\
        a = a + 1;\
        }";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::If {
                cond: Expr::Binary {
                    op: BinaryOp::EqualEqual,
                    left: Box::new(Expr::Variable("a".to_string())),
                    right: Box::new(Expr::Literal(Value::Int(10)))
                },
                then: Block {
                    stmts: vec![
                        Stmt::VarDecl {
                            name: "x".to_string(),
                            init: Some(Expr::Literal(Value::Int(25)))
                        },
                        Stmt::Assign {
                            name: "a".to_string(),
                            expr: Expr::Binary {
                                op: BinaryOp::Plus,
                                left: Box::new(Expr::Variable("a".to_string())),
                                right: Box::new(Expr::Literal(Value::Int(1)))
                            }
                        }
                    ]
                },
                else_branch: None,
            }
        ];
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_if_else() {
        let code = "if(a==10){\
        let x = 25;\
        a = a + 1;\
        } else {\
        a = a * 2;\
        }";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::If {
                cond: Expr::Binary {
                    op: BinaryOp::EqualEqual,
                    left: Box::new(Expr::Variable("a".to_string())),
                    right: Box::new(Expr::Literal(Value::Int(10)))
                },
                then: Block {
                    stmts: vec![
                        Stmt::VarDecl {
                            name: "x".to_string(),
                            init: Some(Expr::Literal(Value::Int(25)))
                        },
                        Stmt::Assign {
                            name: "a".to_string(),
                            expr: Expr::Binary {
                                op: BinaryOp::Plus,
                                left: Box::new(Expr::Variable("a".to_string())),
                                right: Box::new(Expr::Literal(Value::Int(1)))
                            }
                        }
                    ]
                },
                else_branch: Some(Block {
                    stmts: vec![
                        Stmt::Assign {
                            name: "a".to_string(),
                            expr: Expr::Binary {
                                op: BinaryOp::Star,
                                left: Box::new(Expr::Variable("a".to_string())),
                                right: Box::new(Expr::Literal(Value::Int(2)))
                            }
                        }
                    ]
                }),
            }
        ];
        assert_eq!(ast, expected);
    }

    #[test]
    fn test_if_elseif_else() {
        let code = "if(a < 10){\
        a = a + 1;\
        } else if ( a >= 10 && a < 20) {\
        a = a + 2;\
        } else {\
        a = a * 2;\
        }";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::If {
                cond: Expr::Binary {
                    op: BinaryOp::Less,
                    left: Box::new(Expr::Variable("a".to_string())),
                    right: Box::new(Expr::Literal(Value::Int(10)))
                },
                then: Block {
                    stmts: vec![
                        Stmt::Assign {
                            name: "a".to_string(),
                            expr: Expr::Binary {
                                op: BinaryOp::Plus,
                                left: Box::new(Expr::Variable("a".to_string())),
                                right: Box::new(Expr::Literal(Value::Int(1)))
                            }
                        }
                    ]
                },
                else_branch: Some(Block {
                    stmts: vec![
                        Stmt::If {
                            cond: Expr::Binary {
                                op: BinaryOp::And,
                                left: Box::new(Expr::Binary {
                                    op: BinaryOp::GreaterEqual,
                                    left: Box::new(Expr::Variable("a".to_string())),
                                    right: Box::new(Expr::Literal(Value::Int(10)))
                                }),
                                right: Box::new(Expr::Binary {
                                    op: BinaryOp::Less,
                                    left: Box::new(Expr::Variable("a".to_string())),
                                    right: Box::new(Expr::Literal(Value::Int(20)))
                                })
                            },
                            then: Block {
                                stmts: vec![
                                    Stmt::Assign {
                                        name: "a".to_string(),
                                        expr: Expr::Binary {
                                            op: BinaryOp::Plus,
                                            left: Box::new(Expr::Variable("a".to_string())),
                                            right: Box::new(Expr::Literal(Value::Int(2)))
                                        }
                                    }
                                ]
                            },
                            else_branch: Some(Block {
                                stmts: vec![
                                    Stmt::Assign {
                                        name: "a".to_string(),
                                        expr: Expr::Binary {
                                            op: BinaryOp::Star,
                                            left: Box::new(Expr::Variable("a".to_string())),
                                            right: Box::new(Expr::Literal(Value::Int(2)))
                                        }
                                    }
                                ]
                            })
                        }
                    ]
                }),
            }
        ];
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_while() {
        let code = "while(a<10){\
        a = a + 1;\
        }";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::While {
                cond: Expr::Binary {
                    op: BinaryOp::Less,
                    left: Box::new(Expr::Variable("a".to_string())),
                    right: Box::new(Expr::Literal(Value::Int(10)))
                },
                body: Block {
                    stmts: vec![
                        Stmt::Assign {
                            name: "a".to_string(),
                            expr: Expr::Binary {
                                op: BinaryOp::Plus,
                                left: Box::new(Expr::Variable("a".to_string())),
                                right: Box::new(Expr::Literal(Value::Int(1))),
                            }
                        }
                    ]
                }
            }
        ];
        assert_eq!(ast, expected);
    }
    
    #[test]
    fn test_function_decl() {
        let code = "fn sum(a, b) {\
        return a + b;\
        }";
        let tokens = lex(code).unwrap();
        let mut parser = Parser::new(tokens);
        let ast = parser.parse().unwrap();
        let expected = vec![
            Stmt::Function {
                name: "sum".to_string(),
                params: vec!["a".to_string(), "b".to_string()],
                body: Block {
                    stmts: vec![
                        Stmt::Return(Expr::Binary {
                            op: BinaryOp::Plus,
                            left: Box::new(Expr::Variable("a".to_string())),
                            right: Box::new(Expr::Variable("b".to_string()))
                        })
                    ]
                }
            }
        ];
        assert_eq!(ast, expected);
    }
}