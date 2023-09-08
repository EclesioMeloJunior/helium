#![allow(unused)]

pub mod ast;

use ast::{Operator, AST};
use helium_lexer::{
    token::{NumericType, Token, TokenType},
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
    pub fn expression(&mut self) -> Result<AST, String> {
        self.inner_expression(0)
    }

    fn inner_expression(&mut self, min_biding_power: u8) -> Result<AST, String> {
        let mut lhs = match self.token_stream.next() {
            Some(token) => match token.ttype {
                TokenType::Identifier => AST::Identifier(token.lexeme),
                TokenType::Number(n) => AST::from((n, token.lexeme)),
                _ => return Err(format!("unexepected token: {:?}", token)),
            },

            None => return Err(String::from("unexepected end on token stream")),
        };

        loop {
            let operator = match self.token_stream.peek() {
                None => break,
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
    use helium_lexer::token::{NumericType, Token, TokenType};

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
        let output = parser.expression().unwrap();

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
        let output = parser.expression().unwrap();

        assert_eq!(expected, output)
    }
}
