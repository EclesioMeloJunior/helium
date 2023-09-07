use regex::Regex;

use std::collections::HashMap;
use std::convert::TryFrom;
use std::iter::Iterator;

const MULTI_CHAR_TOKENS_MAPPING: [(&'static str, fn(String) -> Token); 3] = [
    (r"^[0-9]+$", |lexeme| -> Token {
        Token {
            lexeme,
            ttype: TokenType::Number(NumericType::Integer),
        }
    }),
    (r"^[0-9]+\.[0-9]+$", |lexeme| -> Token {
        Token {
            lexeme: lexeme,
            ttype: TokenType::Number(NumericType::Float),
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
    Identifier,
    Number(NumericType),

    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub lexeme: String,
    pub ttype: TokenType,
}

impl TryFrom<Vec<char>> for Token {
    type Error = String;
    fn try_from(value: Vec<char>) -> Result<Token, Self::Error> {
        if value.len() < 2 {
            return single_char_token(value.first().unwrap().to_owned());
        }

        multi_char_token(value)
    }
}

fn single_char_token(current_char: char) -> Result<Token, String> {
    match current_char {
        '+' => Ok(Token {
            lexeme: String::from("+"),
            ttype: TokenType::Plus,
        }),
        '-' => Ok(Token {
            lexeme: String::from("-"),
            ttype: TokenType::Minus,
        }),
        '*' => Ok(Token {
            lexeme: String::from("*"),
            ttype: TokenType::Star,
        }),
        '/' => Ok(Token {
            lexeme: String::from("/"),
            ttype: TokenType::Slash,
        }),
        current_char if current_char.is_numeric() => Ok(Token {
            lexeme: String::from(current_char),
            ttype: TokenType::Number(NumericType::Integer),
        }),
        current_char if current_char.is_alphabetic() => Ok(Token {
            lexeme: String::from(current_char),
            ttype: TokenType::Identifier,
        }),
        _ => Err(format!("token not defined: {:?}", current_char)),
    }
}

fn multi_char_token(chars: Vec<char>) -> Result<Token, String> {
    let iter_chars = chars.into_iter();
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
