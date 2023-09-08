#![allow(unused)]

pub mod token;

use std::iter::Iterator as Iter;
use std::iter::Peekable;
use token::{Token, TokenType};

#[derive(Debug, Clone)]
pub struct Lexer<I>
where
    I: Iter<Item = u8>,
{
    stream: Peekable<I>,
}

impl<I> Lexer<I>
where
    I: Iter<Item = u8>,
{
    pub fn new(stream: Peekable<I>) -> Self {
        return Lexer { stream };
    }
}

impl<I> Lexer<I>
where
    I: Iter<Item = u8>,
{
    /// next_term returns a accumulated set of chars until predicate is satisfied
    fn next_while<P: Fn(&I::Item) -> bool>(&mut self, predicate: P) -> Vec<char> {
        let mut acc: Vec<char> = vec![];

        while let Some(token) = self
            .stream
            .next_if(&predicate)
            .map_or(None, |i| Some(i as char))
        {
            acc.push(token)
        }

        acc
    }
}

impl From<String> for Lexer<std::vec::IntoIter<u8>> {
    fn from(value: String) -> Self {
        return Lexer {
            stream: value.into_bytes().into_iter().peekable(),
        };
    }
}

impl<I> Iter for Lexer<I>
where
    I: Iter<Item = u8>,
{
    type Item = Result<Token, String>;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(stream_item) = self.stream.next().map_or(None, |i| Some(i as char)) {
            match stream_item {
                ' ' | '\n' | '\r' | '\t' => continue,
                '+' => {
                    return Some(Ok(Token {
                        lexeme: String::from("+"),
                        ttype: TokenType::Plus,
                    }))
                }
                '-' => {
                    return Some(Ok(Token {
                        lexeme: String::from("-"),
                        ttype: TokenType::Minus,
                    }))
                }
                '*' => {
                    return Some(Ok(Token {
                        lexeme: String::from("*"),
                        ttype: TokenType::Star,
                    }))
                }
                '/' => {
                    return Some(Ok(Token {
                        lexeme: String::from("/"),
                        ttype: TokenType::Slash,
                    }))
                }
                '(' => {
                    return Some(Ok(Token {
                        lexeme: String::from("("),
                        ttype: TokenType::OpenParen,
                    }))
                }
                ')' => {
                    return Some(Ok(Token {
                        lexeme: String::from(")"),
                        ttype: TokenType::CloseParen,
                    }))
                }
                _ => {
                    let mut full_term: Vec<char> = vec![stream_item];
                    if stream_item.is_alphabetic() || stream_item == '_' {
                        full_term.extend(
                            self.next_while(|i| {
                                (*i as char).is_alphabetic() || (*i as char) == '_'
                            }),
                        );
                    }

                    if stream_item.is_numeric() {
                        full_term.extend(
                            self.next_while(|i| (*i as char).is_numeric() || (*i as char) == '.'),
                        );
                    }

                    //println!(">>{:?}", full_term);
                    return Some(Token::try_from(full_term));
                }
            }
        }

        None
    }
}

mod tests {
    use crate::token::{NumericType, Token, TokenType};
    use crate::Lexer;

    #[test]
    fn read_stream_of_tokens() {
        let tokens_as_str = String::from("(0-1) 12 + 1 helium h_e_lium 1.22");
        let expected: Vec<Token> = vec![
            Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
            },
            Token {
                lexeme: String::from("0"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from("-"),
                ttype: TokenType::Minus,
            },
            Token {
                lexeme: String::from("1"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from(")"),
                ttype: TokenType::CloseParen,
            },
            Token {
                lexeme: String::from("12"),
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
                lexeme: String::from("helium"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from("h_e_lium"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from("1.22"),
                ttype: TokenType::Number(NumericType::Float),
            },
        ];

        let lexer = Lexer::from(tokens_as_str);
        assert_eq!(lexer.clone().count(), expected.len() as usize);

        for (expected, got) in expected.into_iter().zip(lexer.map(|r| r.unwrap())) {
            assert_eq!(expected, got);
        }
    }

    #[test]
    fn read_stream_with_error() {
        let input = String::from("1.209.90");
        let mut lexer = Lexer::from(input);

        let token = lexer.next();
        assert!(token.is_some());

        let token_result = token.unwrap();
        assert!(token_result.is_err());

        let err = token_result.unwrap_err();
        assert_eq!(err, String::from("token not defined: \"1.209.90\""))
    }
}
