#![allow(unused)]

pub mod ast;

use ast::{Operator, AST};
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
            .next_if(|inner_token| match inner_token.ttype {
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
            .next_if(|inner_token| match inner_token.ttype {
                TokenType::OpenParen => true,
                _ => false,
            })
            .expect(
                format!(
                    "expected function open parenthesis, got {:?}",
                    self.token_stream.peek()
                )
                .as_str(),
            );

        // parser function arity
        let mut fn_arity: Vec<String> = vec![];
        loop {
            match self.token_stream.peek() {
                None => {
                    return Err(String::from(
                        "EOF not expected while parsing function literal",
                    ))
                }
                Some(token) if token.ttype == TokenType::CloseParen => break,
                Some(token) if token.ttype == TokenType::Identifier => {
                    fn_arity.push(token.lexeme.to_string())
                }
                Some(token) => return Err(format!("expected function arguments, got {:?}", token)),
            }
        }

        // TODO: include `;` in the lexer, then will be a lot easier
        // to delimit a expression/statement inside a funtion
        let mut fn_body: Vec<Box<AST>> = vec![];

        loop {}

        Ok(AST::FunctionLiteral {
            name: fn_identifier.lexeme.to_string(),
            args: fn_arity,
            body: fn_body,
        })
    }

    fn inner_expression(&mut self, min_biding_power: u8) -> Result<AST, String> {
        let mut lhs = match self.token_stream.next() {
            Some(token) => match token.ttype {
                TokenType::Func => match self.parse_function_literal() {
                    Ok(ast) => ast,
                    Err(err) => return Err(err),
                },
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
                Some(token) if token.ttype == TokenType::CloseParen => break,
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
        ast::{Operator, AST},
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
        let program = String::from("func main() { return 1 + 1}");
        let lexer = Lexer::from(program);
        let lexer = lexer.map(|token_result| token_result.unwrap()).peekable();
        let mut parser = Parser::new(lexer);

        let expected = AST::FunctionLiteral {
            name: String::from("main"),
            args: vec![],
            body: vec![Box::new(AST::ReturnStatement(Box::new(
                AST::BinaryExpression {
                    operator: Operator::Plus,
                    lhs: Box::new(AST::Integer(1)),
                    rhs: Box::new(AST::Integer(1)),
                },
            )))],
        };

        let got_ast = parser.program().unwrap();
        assert_eq!(expected, got_ast);
    }
}
