use regex::Regex;

use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::Iterator;

const MULTI_CHAR_TOKENS_MAPPING: [(&'static str, fn(String) -> Token); 12] = [
    (r"^(i32|f32|bool)$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Type,
        }
    }),
    (r"^void$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Void,
        }
    }),
    (r"let$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Let,
        }
    }),
    (r"return$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Return,
        }
    }),
    (r"func$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Func,
        }
    }),
    (r"^if$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::If,
        }
    }),
    (r"^else$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Else,
        }
    }),
    (r"^true$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::True,
        }
    }),
    (r"^false$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::False,
        }
    }),
    (r"^[0-9]+\.[0-9]+$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Number(NumericType::Float),
        }
    }),
    (r"^[0-9]+$", |lexeme| -> Token {
        Token {
            lexeme,
            ttype: TokenType::Number(NumericType::Integer),
        }
    }),
    (r"^[a-zA-Z_]+$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Identifier,
        }
    }),
];

#[derive(Debug, PartialEq)]
pub enum NumericType {
    Integer,
    Float,
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Type,
    Identifier,
    Number(NumericType),

    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Semicolon,

    Colon,
    OpenParen,
    CloseParen,
    OpenBrackets,
    CloseBrackets,

    Let,
    Func,
    Return,
    Void,
    If,
    Else,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Equals,

    True,
    False,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub lexeme: String,
    pub ttype: TokenType,
}

impl TryFrom<Vec<char>> for Token {
    type Error = String;
    fn try_from(value: Vec<char>) -> Result<Token, Self::Error> {
        let iter_chars = value.into_iter();
        let lexeme = iter_chars.collect::<String>();

        for (rgx_string, constructor) in MULTI_CHAR_TOKENS_MAPPING {
            let re = Regex::new(rgx_string).unwrap();
            match re.is_match(&lexeme) {
                true => return Ok(constructor(lexeme)),
                false => continue,
            }
        }

        return Err(format!("token not defined: {:?}", lexeme));
    }
}
