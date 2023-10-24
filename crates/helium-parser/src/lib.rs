#![allow(unused)]

pub mod ast;

use ast::{Operator, Type, AST};
use helium_lexer::{
    token::{self, NumericType, Token, TokenType},
    Lexer,
};
use std::{
    fmt::Binary,
    iter::{Iterator, Peekable},
};

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    token_stream: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(token_stream: Peekable<I>) -> Self {
        Parser { token_stream }
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn program(&mut self) -> Result<AST, String> {
        self.inner_expression(0)
    }

    fn parse_function_literal(&mut self) -> Result<AST, String> {
        // parse the function identifier
        let fn_identifier = self
            .token_stream
            .next_if(|inner_token: &Token| match inner_token.ttype {
                TokenType::Identifier => true,
                _ => false,
            })
            .expect(
                format!(
                    "expected function identifier, got {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        // parse the first open paren
        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
            })
            .expect(
                format!(
                    "expected function open parenthesis, got {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        // parser function arity
        let mut fn_arity: Vec<(String, Type)> = vec![];
        loop {
            match self.token_stream.next() {
                Some(token) if token.ttype == TokenType::CloseParen => break,
                Some(token) if token.ttype == TokenType::Colon => continue,
                Some(token) if token.ttype == TokenType::Identifier => {
                    let argument_identifier = token.lexeme.to_string();

                    self.token_stream
                        .next_if_eq(&Token {
                            lexeme: String::from(":"),
                            ttype: TokenType::Colon,
                        })
                        .expect(
                            format!("expected colon, got {:?}", self.token_stream.peek()).as_str(),
                        );

                    let argument_type = match self.token_stream.next() {
                        Some(value) => Type::try_from(value)?,
                        None => {
                            return Err(String::from("expected argument type definition, got None"))
                        }
                    };

                    fn_arity.push((argument_identifier, argument_type));
                }
                Some(token) => return Err(format!("expected function arguments, got {:?}", token)),
                None => {
                    return Err(String::from(
                        "EOF not expected while parsing function arity",
                    ))
                }
            }
        }

        // parse the return type
        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            })
            .expect(format!("expected colon (:), got {:?}", self.token_stream.peek()).as_str());

        let return_type = Type::try_from(self.token_stream.next().unwrap()).unwrap();

        // parse the first open brackets
        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from("{"),
                ttype: TokenType::OpenBrackets,
            })
            .expect(
                format!(
                    "expected function open brackets, got {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        let mut fn_body: Vec<Box<AST>> = vec![];

        loop {
            match self.token_stream.peek() {
                None => return Err(String::from("EOF not expected while parsing function body")),
                Some(token) if token.ttype == TokenType::CloseBrackets => {
                    self.token_stream.next().unwrap();
                    break;
                }
                _ => fn_body.push(Box::new(self.inner_expression(0)?)),
            }
        }

        Ok(AST::FunctionLiteral {
            name: fn_identifier.lexeme.to_string(),
            args: fn_arity,
            return_type: return_type,
            body: fn_body,
        })
    }

    fn parse_let_statment(&mut self) -> Result<AST, String> {
        let identifier = self
            .token_stream
            .next_if(|inner_token| match inner_token.ttype {
                TokenType::Identifier => true,
                _ => false,
            });

        if identifier.is_none() {
            return Err(format!(
                "expected variable declaration, got {:?}",
                self.token_stream.peek()
            ));
        }

        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            })
            .expect(format!("expect colon (:), got {:?}", self.token_stream.peek()).as_str());

        let let_stmt_type = Type::try_from(self.token_stream.next().unwrap()).unwrap();

        if self
            .token_stream
            .next_if_eq(&Token {
                lexeme: String::from("="),
                ttype: TokenType::Assign,
            })
            .is_none()
        {
            return Err(format!(
                "expected assignment symbol '=', got {:?}",
                self.token_stream.peek()
            ));
        }

        let assignment_expression = self.inner_expression(0)?;

        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from(";"),
                ttype: TokenType::Semicolon,
            })
            .expect(
                format!(
                    "let statment should end with `;` (semicolon), got {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        Ok(AST::LetStatment {
            var_type: let_stmt_type,
            variable: identifier.unwrap().lexeme,
            rhs: Box::new(assignment_expression),
        })
    }

    fn parse_conditional_statement(&mut self) -> Result<AST, String> {
        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
            })
            .expect(
                format!(
                    "expected open parentheses '(', got: {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        let guard_expression = self.inner_expression(0)?;

        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from(")"),
                ttype: TokenType::CloseParen,
            })
            .expect(
                format!(
                    "expected closing parentheses ')', got: {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        self.token_stream
            .next_if_eq(&Token {
                lexeme: String::from("{"),
                ttype: TokenType::OpenBrackets,
            })
            .expect(
                format!(
                    "expected open brackets '{{', got: {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        let mut body: Vec<Box<AST>> = vec![];

        loop {
            match self.token_stream.peek() {
                None => {
                    return Err(String::from(
                        "EOF not expected while parshing if condition body",
                    ))
                }
                Some(token) if token.ttype == TokenType::CloseBrackets => {
                    self.token_stream.next().unwrap();
                    break;
                }
                _ => body.push(Box::new(self.inner_expression(0)?)),
            }
        }

        let else_body = {
            // if the next token is an else then we parse its body
            match self.token_stream.next_if_eq(&Token {
                lexeme: String::from("else"),
                ttype: TokenType::Else,
            }) {
                None => None,
                _ => {
                    self.token_stream
                        .next_if_eq(&Token {
                            lexeme: String::from("{"),
                            ttype: TokenType::OpenBrackets,
                        })
                        .expect(
                            format!(
                                "expected open brackets '{{', got: {:?}",
                                self.token_stream.peek()
                            )
                            .as_str(),
                        );

                    let mut else_body: Vec<Box<AST>> = vec![];
                    loop {
                        match self.token_stream.peek() {
                            None => {
                                return Err(String::from(
                                    "EOF not expected while parshing if condition body",
                                ))
                            }
                            Some(token) if token.ttype == TokenType::CloseBrackets => {
                                self.token_stream.next().unwrap();
                                break;
                            }
                            _ => else_body.push(Box::new(self.inner_expression(0)?)),
                        }
                    }

                    Some(else_body)
                }
            }
        };

        Ok(AST::IfStatement {
            body: body,
            guard: Box::new(guard_expression),
            else_body: else_body,
        })
    }

    fn inner_expression(&mut self, min_biding_power: u8) -> Result<AST, String> {
        let mut lhs = match self.token_stream.next() {
            Some(token) => match token.ttype {
                TokenType::Func => return self.parse_function_literal(),
                TokenType::Let => return self.parse_let_statment(),
                TokenType::If => return self.parse_conditional_statement(),
                TokenType::Return => {
                    return Ok(AST::ReturnStatement(Box::new(self.inner_expression(0)?)))
                }
                TokenType::Identifier => AST::Identifier(token.lexeme),
                TokenType::Number(n) => AST::from((n, token.lexeme)),
                TokenType::OpenParen => {
                    let lhs = self.inner_expression(0)?;
                    match self.token_stream.next() {
                        None => return Err(String::from("unexpected end of token stream")),
                        Some(t) if t.ttype == TokenType::CloseParen => lhs,
                        Some(t) => {
                            return Err(format!(
                                "expected: ')' (CloseParen)\ngot: '{}' ({:?})",
                                t.lexeme, t.ttype,
                            ))
                        }
                    }
                }
                _ => {
                    let prefix_op = Operator::try_from(&token)?;
                    match prefix_op.prefix_biding_power() {
                        Ok(((), r_bp)) => {
                            let rhs = self.inner_expression(r_bp)?;
                            AST::UnaryExpression {
                                operator: prefix_op,
                                expression: Box::new(rhs),
                            }
                        }
                        Err(err) => return Err(format!("unexepected token: {}", err)),
                    }
                }
            },

            None => return Err(String::from("unexepected end on token stream")),
        };

        loop {
            let operator = match self.token_stream.peek() {
                None => break,
                Some(token)
                    if token.ttype == TokenType::Semicolon
                        || token.ttype == TokenType::CloseParen
                        || token.ttype == TokenType::CloseBrackets =>
                {
                    break
                }
                Some(token) => Operator::try_from(token)?,
            };

            let (left_bindpower, right_bindpower) = operator.infix_biding_power()?;
            if left_bindpower < min_biding_power {
                break;
            }

            self.token_stream.next().unwrap(); // ingest the operator
            let rhs = self.inner_expression(right_bindpower)?;

            lhs = AST::BinaryExpression {
                operator: operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }
}

mod tests {
    use std::fmt::Binary;

    use helium_lexer::{
        token::{NumericType, Token, TokenType},
        Lexer,
    };

    use crate::{
        ast::{Operator, Type, AST},
        Parser,
    };

    #[test]
    fn parse_simple_sum() {
        let tokens = vec![
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from("+"),
                ttype: TokenType::Plus,
            },
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
        ];

        let expected = AST::BinaryExpression {
            operator: Operator::Plus,
            lhs: Box::new(AST::Integer(1)),
            rhs: Box::new(AST::Integer(1)),
        };

        let mut parser = Parser::new(tokens.into_iter().peekable());
        let output = parser.program().unwrap();

        assert_eq!(expected, output)
    }

    #[test]
    fn parse_simple_comparision() {
        let tokens = vec![
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from(">"),
                ttype: TokenType::Greater,
            },
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
        ];

        let expected = AST::BinaryExpression {
            operator: Operator::Greater,
            lhs: Box::new(AST::Integer(1)),
            rhs: Box::new(AST::Integer(1)),
        };

        let mut parser = Parser::new(tokens.into_iter().peekable());
        let output = parser.program().unwrap();

        assert_eq!(expected, output)
    }

    #[test]
    fn parse_arithmetic_precendence() {
        let tokens = vec![
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from("+"),
                ttype: TokenType::Plus,
            },
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from("*"),
                ttype: TokenType::Star,
            },
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
        ];

        let expected = AST::BinaryExpression {
            operator: Operator::Plus,
            lhs: Box::new(AST::Integer(1)),
            rhs: Box::new(AST::BinaryExpression {
                operator: Operator::Star,
                lhs: Box::new(AST::Integer(1)),
                rhs: Box::new(AST::Integer(1)),
            }),
        };

        let mut parser = Parser::new(tokens.into_iter().peekable());
        let output = parser.program().unwrap();

        assert_eq!(expected, output)
    }

    #[test]
    fn parse_unary_in_expression() {
        // ((- 1) + (2 * 3) - 3))
        let tokens = vec![
            Token {
                lexeme: String::from("-"),
                ttype: TokenType::Minus,
            },
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from("+"),
                ttype: TokenType::Plus,
            },
            Token {
                lexeme: String::from("2"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from("*"),
                ttype: TokenType::Star,
            },
            Token {
                lexeme: String::from("3"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from("-"),
                ttype: TokenType::Star,
            },
            Token {
                lexeme: String::from("3"),
                ttype: TokenType::Number(NumericType::Integer),
            },
        ];

        let expected_ast = AST::BinaryExpression {
            operator: Operator::Plus,
            lhs: Box::new(AST::UnaryExpression {
                operator: Operator::Minus,
                expression: Box::new(AST::Integer(1)),
            }),
            rhs: Box::new(AST::BinaryExpression {
                operator: Operator::Minus,
                lhs: Box::new(AST::BinaryExpression {
                    operator: Operator::Star,
                    lhs: Box::new(AST::Integer(2)),
                    rhs: Box::new(AST::Integer(3)),
                }),
                rhs: Box::new(AST::Integer(3)),
            }),
        };

        let mut parser = Parser::new(tokens.into_iter().peekable());
        let ast = parser.program();
    }

    #[test]
    fn parse_parenthesis_expression() {
        let program = String::from("(1 + 1) * -2");
        let lexer = Lexer::from(program);
        let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();
        let mut parser = Parser::new(lexer);

        let expected = AST::BinaryExpression {
            operator: Operator::Star,
            lhs: Box::new(AST::BinaryExpression {
                operator: Operator::Plus,
                lhs: Box::new(AST::Integer(1)),
                rhs: Box::new(AST::Integer(1)),
            }),
            rhs: Box::new(AST::UnaryExpression {
                operator: Operator::Minus,
                expression: Box::new(AST::Integer(2)),
            }),
        };

        let got_ast = parser.program().unwrap();
        assert_eq!(expected, got_ast);
    }

    #[test]
    fn parse_main_function() {
        let program = String::from(
            "func main() : i32 { 
                let a : i32 = 1 + 1;
                return a
            }",
        );
        let lexer = Lexer::from(program);
        let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();

        let mut parser = Parser::new(lexer);

        let expected = AST::FunctionLiteral {
            name: String::from("main"),
            return_type: Type::I32,
            args: vec![],
            body: vec![
                Box::new(AST::LetStatment {
                    var_type: Type::I32,
                    variable: String::from("a"),
                    rhs: Box::new(AST::BinaryExpression {
                        operator: Operator::Plus,
                        lhs: Box::new(AST::Integer(1)),
                        rhs: Box::new(AST::Integer(1)),
                    }),
                }),
                Box::new(AST::ReturnStatement(Box::new(AST::Identifier(
                    String::from("a"),
                )))),
            ],
        };

        let got_ast = parser.program().unwrap();
        assert_eq!(expected, got_ast);
    }

    #[test]
    fn parse_function_with_args() {
        let program = String::from(
            "func main(a: i32, b: i32) : i32 { 
                let c : i32 = a + b;
                return c
            }",
        );
        let lexer = Lexer::from(program);
        let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();
        let mut parser = Parser::new(lexer);

        let expected = AST::FunctionLiteral {
            return_type: Type::I32,
            name: String::from("main"),
            args: vec![
                (String::from("a"), Type::I32),
                (String::from("b"), Type::I32),
            ],
            body: vec![
                Box::new(AST::LetStatment {
                    var_type: Type::I32,
                    variable: String::from("c"),
                    rhs: Box::new(AST::BinaryExpression {
                        operator: Operator::Plus,
                        lhs: Box::new(AST::Identifier(String::from("a"))),
                        rhs: Box::new(AST::Identifier(String::from("b"))),
                    }),
                }),
                Box::new(AST::ReturnStatement(Box::new(AST::Identifier(
                    String::from("c"),
                )))),
            ],
        };

        let got_ast = parser.program().unwrap();
        assert_eq!(expected, got_ast);
    }

    #[test]
    fn parse_conditional_flow() {
        let program = String::from(
            "
            func main(a: i32, b: i32) : i32 {
                if (a < b) {
                    return 1
                } else {
                    return 2
                }
            }
        ",
        );

        let lexer = Lexer::from(program);
        let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();
        let mut parser = Parser::new(lexer);

        let expected = AST::FunctionLiteral {
            name: String::from("main"),
            args: vec![
                (String::from("a"), Type::I32),
                (String::from("b"), Type::I32),
            ],
            return_type: Type::I32,
            body: vec![Box::new(AST::IfStatement {
                guard: Box::new(AST::BinaryExpression {
                    operator: Operator::Less,
                    lhs: Box::new(AST::Identifier(String::from("a"))),
                    rhs: Box::new(AST::Identifier(String::from("b"))),
                }),
                body: vec![Box::new(AST::ReturnStatement(Box::new(AST::Integer(1))))],
                else_body: Some(vec![Box::new(AST::ReturnStatement(Box::new(
                    AST::Integer(2),
                )))]),
            })],
        };

        let got_ast = parser.program().unwrap();
        assert_eq!(expected, got_ast);
    }
}
