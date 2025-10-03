use crate::ast::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    VarDecl,
    Function,
    Return,
    If,
    Else,
    While,
    For,
    True,
    False,
    Type(Type),

    Identifier(String),
    Int(i64),
    Float(f64),
    Char(char),
    String(String),

    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    DoubleEqual,
    Not,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Xor,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LShift,
    RShift,

    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Colon,
    ArrRight,
    
    EOF
}

pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            ' ' | '\t' | '\n' | '\r' => continue,
            _ if c.is_alphabetic() || c == '_' => {
                let mut buffer = String::from(c);
                while let Some(&c) = chars.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        buffer.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                match buffer.as_str() {
                    "let" => tokens.push(Token::VarDecl),
                    "fn" => tokens.push(Token::Function),
                    "return" => tokens.push(Token::Return),
                    "if" => tokens.push(Token::If),
                    "else" => tokens.push(Token::Else),
                    "while" => tokens.push(Token::While),
                    "true" => tokens.push(Token::True),
                    "false" => tokens.push(Token::False),
                    "int" | "i64" => tokens.push(Token::Type(Type::Int)),
                    "float" | "f64" => tokens.push(Token::Type(Type::Float)),
                    "string" | "str" => tokens.push(Token::Type(Type::String)),
                    "char" => tokens.push(Token::Type(Type::Char)),
                    "bool" => tokens.push(Token::Type(Type::Bool)),
                    "void" => tokens.push(Token::Type(Type::Void)),
                    _ => tokens.push(Token::Identifier(buffer)),
                }
            },
            _ if c.is_numeric() => {
                let mut buffer = String::from(c);
                while let Some(&c) = chars.peek() {
                    if c.is_numeric() || c == '_' || c == '.' {
                        buffer.push(c);
                        chars.next();
                    } else {
                        break;
                    }
                }
                buffer = buffer.replace("_", "");
                if buffer.contains(".") {
                    tokens.push(Token::Float(match buffer.parse::<f64>() {
                        Ok(f) => f,
                        Err(_) => return Err(format!("Invalid float number: {}", buffer)),
                    }));
                } else {
                    tokens.push(Token::Int(match buffer.parse::<i64>() {
                        Ok(i) => i,
                        Err(_) => return Err(format!("Invalid integer number: {}", buffer)),
                    }))
                }
            },
            '\'' => {
                let mut buffer = String::new();
                let mut is_quotes_closed = false;
                
                while let Some(&c) = chars.peek() {
                    if c == '\'' {
                        chars.next();
                        is_quotes_closed = true;
                        break;
                    } else {
                        buffer.push(c);
                        chars.next();
                    }
                }
                if !is_quotes_closed {
                    return Err("Unexpected end of quotes!".to_string());
                }

                if buffer.len() == 1 {
                    tokens.push(Token::Char(buffer.as_bytes()[0] as char))
                } else {
                    tokens.push(Token::String(buffer));
                }
            }
            
            '\"' => {
                let mut s = String::new();
                let mut is_quotes_closed = false;
                while let Some(&c) = chars.peek() {
                    if c == '\"' {
                        chars.next();
                        is_quotes_closed = true;
                        break;
                    } else {
                        s.push(c);
                        chars.next();
                    }
                }
                if !is_quotes_closed {
                    return Err("Unexpected end of quotes!".to_string());
                }

                if s.len() == 1 {
                    tokens.push(Token::Char(s.as_bytes()[0] as char))
                } else {
                    tokens.push(Token::String(s));
                }
            },
            '+' => tokens.push(Token::Plus),
            '-' => {
                if let Some(&'>') = chars.peek() {
                    chars.next();
                    tokens.push(Token::ArrRight);
                } else {
                    tokens.push(Token::Minus);
                }
            },
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            '=' => {
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::DoubleEqual);
                } else {
                    tokens.push(Token::Equal);
                }
            }
            '!' => {
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::NotEqual);
                } else {
                    tokens.push(Token::Not);
                }
            }
            '<' => {
                match chars.peek() {
                    Some(&'=') => {
                        chars.next();
                        tokens.push(Token::LessEqual);
                    },
                    Some(&'<') => {
                        chars.next();
                        tokens.push(Token::LShift);
                    },
                    Some(_) | None => {
                        tokens.push(Token::Less);
                    }
                }
            }
            '>' => {
                match chars.peek() {
                    Some(&'=') => {
                        chars.next();
                        tokens.push(Token::GreaterEqual);
                    },
                    Some(&'>') => {
                        chars.next();
                        tokens.push(Token::RShift);
                    },
                    Some(_) | None => {
                        tokens.push(Token::Greater);
                    }
                }
            }
            '&' => {
                if let Some(&'&') = chars.peek() {
                    chars.next();
                    tokens.push(Token::And);
                } else {
                    chars.next();
                    tokens.push(Token::BitwiseAnd);
                }
            }
            '|' => {
                if let Some(&'|') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Or);
                } else {
                    chars.next();
                    tokens.push(Token::BitwiseOr);
                }
            }
            '^' => {
                if let Some(&'^') = chars.peek() {
                    tokens.push(Token::Xor);
                } else {
                    tokens.push(Token::BitwiseXor);
                }
            }
            '~' => {
                chars.next();
                tokens.push(Token::BitwiseNot);
            }
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            ',' => tokens.push(Token::Comma),
            ';' => tokens.push(Token::Semicolon),
            ':' => tokens.push(Token::Colon),

            _ => {
                return Err(format!("Invalid character '{}'", c));
            }
        }
    }
    tokens.push(Token::EOF);
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_func() {
        let code = "
        fn main() {
            let a = 10;
            let b = 5;
            let c = '+';
            let d = calculate(a, b, c);
            return d + calculate(20, 9, '-');
        }

        fn calculate(a, b, c) {
            if(c == '+') {
                return a + b;
            } else {
                return a - b;
            }
            return -1;
        } ";
    let tokens = lex(&code).unwrap();
    println!("{tokens:?}");
    }
}