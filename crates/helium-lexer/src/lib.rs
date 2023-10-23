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
                '{' => {
                    return Some(Ok(Token {
                        lexeme: String::from("{"),
                        ttype: TokenType::OpenBrackets,
                    }))
                }
                '}' => {
                    return Some(Ok(Token {
                        lexeme: String::from("}"),
                        ttype: TokenType::CloseBrackets,
                    }))
                }
                ';' => {
                    return Some(Ok(Token {
                        lexeme: String::from(";"),
                        ttype: TokenType::Semicolon,
                    }))
                }
                ',' => {
                    return Some(Ok(Token {
                        lexeme: String::from(","),
                        ttype: TokenType::Colon,
                    }))
                }
                '>' => match self.stream.next_if_eq(&('=' as u8)) {
                    Some(_) => {
                        return Some(Ok(Token {
                            lexeme: String::from(">="),
                            ttype: TokenType::GreaterOrEqual,
                        }))
                    }
                    None => {
                        return Some(Ok(Token {
                            lexeme: String::from(">"),
                            ttype: TokenType::Greater,
                        }))
                    }
                },
                '<' => match self.stream.next_if_eq(&('=' as u8)) {
                    Some(_) => {
                        return return Some(Ok(Token {
                            lexeme: String::from("<="),
                            ttype: TokenType::LessOrEqual,
                        }))
                    }
                    None => {
                        return Some(Ok(Token {
                            lexeme: String::from("<"),
                            ttype: TokenType::Less,
                        }))
                    }
                },
                '=' => match self.stream.next_if_eq(&('=' as u8)) {
                    Some(_) => {
                        return Some(Ok(Token {
                            lexeme: String::from("=="),
                            ttype: TokenType::Equals,
                        }))
                    }
                    None => {
                        return Some(Ok(Token {
                            lexeme: String::from("="),
                            ttype: TokenType::Assign,
                        }))
                    }
                },
                ':' => {
                    return Some(Ok(Token {
                        lexeme: String::from(":"),
                        ttype: TokenType::Colon,
                    }))
                }
                _ => {
                    let mut full_term: Vec<char> = vec![stream_item];
                    if stream_item.is_alphanumeric() || stream_item == '_' {
                        full_term.extend(self.next_while(|i| {
                            (*i as char).is_alphanumeric()
                                || (*i as char) == '_'
                                || (*i as char) == '.'
                        }));
                    }

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

    fn tokens_comparision_helper(input: String, expected: Vec<Token>) -> bool {
        let input = String::from(input);
        let mut lexer = Lexer::from(input);

        assert_eq!(lexer.clone().count(), expected.len() as usize);

        expected
            .into_iter()
            .zip(lexer.map(|r| r.unwrap()))
            .map(|(expected, got)| expected == got)
            .all(|item| item)
    }

    #[test]
    fn read_stream_of_tokens() {
        let input =
            String::from("(0-1) 12 + 1 helium h_e_lium 1.22 printf(1) return func main() {} : i32");
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
            Token {
                lexeme: String::from("printf"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
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
                lexeme: String::from("return"),
                ttype: TokenType::Return,
            },
            Token {
                lexeme: String::from("func"),
                ttype: TokenType::Func,
            },
            Token {
                lexeme: String::from("main"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
            },
            Token {
                lexeme: String::from(")"),
                ttype: TokenType::CloseParen,
            },
            Token {
                lexeme: String::from("{"),
                ttype: TokenType::OpenBrackets,
            },
            Token {
                lexeme: String::from("}"),
                ttype: TokenType::CloseBrackets,
            },
            Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            },
            Token {
                lexeme: String::from("i32"),
                ttype: TokenType::Type,
            },
        ];

        assert!(tokens_comparision_helper(input, expected));
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

    #[test]
    fn read_statement_with_semicolon() {
        let input = String::from("let a=10;");

        let expected = vec![
            Token {
                lexeme: String::from("let"),
                ttype: TokenType::Let,
            },
            Token {
                lexeme: String::from("a"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from("="),
                ttype: TokenType::Assign,
            },
            Token {
                lexeme: String::from("10"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from(";"),
                ttype: TokenType::Semicolon,
            },
        ];

        assert!(tokens_comparision_helper(input, expected));
    }

    #[test]
    fn parsing_types_variables_functions() {
        let input = String::from("let a : i32 = 10;");
        let expected = vec![
            Token {
                lexeme: String::from("let"),
                ttype: TokenType::Let,
            },
            Token {
                lexeme: String::from("a"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            },
            Token {
                lexeme: String::from("i32"),
                ttype: TokenType::Type,
            },
            Token {
                lexeme: String::from("="),
                ttype: TokenType::Assign,
            },
            Token {
                lexeme: String::from("10"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from(";"),
                ttype: TokenType::Semicolon,
            },
        ];

        assert!(tokens_comparision_helper(input, expected));

        let input = String::from(
            "func main(a: f32) : i32 {
                return 0;
            }",
        );
        let expected = vec![
            Token {
                lexeme: String::from("func"),
                ttype: TokenType::Func,
            },
            Token {
                lexeme: String::from("main"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
            },
            Token {
                lexeme: String::from("a"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            },
            Token {
                lexeme: String::from("f32"),
                ttype: TokenType::Type,
            },
            Token {
                lexeme: String::from(")"),
                ttype: TokenType::CloseParen,
            },
            Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            },
            Token {
                lexeme: String::from("i32"),
                ttype: TokenType::Type,
            },
            Token {
                lexeme: String::from("{"),
                ttype: TokenType::OpenBrackets,
            },
            Token {
                lexeme: String::from("return"),
                ttype: TokenType::Return,
            },
            Token {
                lexeme: String::from("0"),
                ttype: TokenType::Number(NumericType::Integer),
            },
            Token {
                lexeme: String::from(";"),
                ttype: TokenType::Semicolon,
            },
            Token {
                lexeme: String::from("}"),
                ttype: TokenType::CloseBrackets,
            },
        ];

        assert!(tokens_comparision_helper(input, expected));

        let input = String::from(
            "func main(a: f32) : void {
            }",
        );
        let expected = vec![
            Token {
                lexeme: String::from("func"),
                ttype: TokenType::Func,
            },
            Token {
                lexeme: String::from("main"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
            },
            Token {
                lexeme: String::from("a"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            },
            Token {
                lexeme: String::from("f32"),
                ttype: TokenType::Type,
            },
            Token {
                lexeme: String::from(")"),
                ttype: TokenType::CloseParen,
            },
            Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            },
            Token {
                lexeme: String::from("void"),
                ttype: TokenType::Void,
            },
            Token {
                lexeme: String::from("{"),
                ttype: TokenType::OpenBrackets,
            },
            Token {
                lexeme: String::from("}"),
                ttype: TokenType::CloseBrackets,
            },
        ];

        assert!(tokens_comparision_helper(input, expected));
    }

    #[test]
    fn boolean_type_and_conditional_flow() {
        let input = String::from(
            "
            let a: bool = true;
            if (a) { }
            else { }
        ",
        );

        let expected = vec![
            Token {
                lexeme: String::from("let"),
                ttype: TokenType::Let,
            },
            Token {
                lexeme: String::from("a"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from(":"),
                ttype: TokenType::Colon,
            },
            Token {
                lexeme: String::from("bool"),
                ttype: TokenType::Type,
            },
            Token {
                lexeme: String::from("="),
                ttype: TokenType::Assign,
            },
            Token {
                lexeme: String::from("true"),
                ttype: TokenType::True,
            },
            Token {
                lexeme: String::from(";"),
                ttype: TokenType::Semicolon,
            },
            Token {
                lexeme: String::from("if"),
                ttype: TokenType::If,
            },
            Token {
                lexeme: String::from("("),
                ttype: TokenType::OpenParen,
            },
            Token {
                lexeme: String::from("a"),
                ttype: TokenType::Identifier,
            },
            Token {
                lexeme: String::from(")"),
                ttype: TokenType::CloseParen,
            },
            Token {
                lexeme: String::from("{"),
                ttype: TokenType::OpenBrackets,
            },
            Token {
                lexeme: String::from("}"),
                ttype: TokenType::CloseBrackets,
            },
            Token {
                lexeme: String::from("else"),
                ttype: TokenType::Else,
            },
            Token {
                lexeme: String::from("{"),
                ttype: TokenType::OpenBrackets,
            },
            Token {
                lexeme: String::from("}"),
                ttype: TokenType::CloseBrackets,
            },
        ];

        assert!(tokens_comparision_helper(input, expected))
    }
}
