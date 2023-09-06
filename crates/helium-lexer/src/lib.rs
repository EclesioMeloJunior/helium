#![allow(unused)]

mod token;

use std::iter::Iterator as Iter;
use std::iter::Peekable;
use token::{Token, TokenType};

#[derive(Debug, Clone)]
struct Lexer<I>
where
    I: Iter<Item = u8>,
{
    stream: Peekable<I>,
}

impl<I> Lexer<I>
where
    I: Iter<Item = u8>,
{
    fn new(stream: Peekable<I>) -> Self {
        return Lexer { stream };
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
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(stream_item) = self.stream.next().map_or(None, |i| Some(i as char)) {
            match stream_item {
                ' ' | '\n' | '\r' | '\t' => continue,
                _ => {
                    return Some(Token {
                        lexeme: stream_item.to_string(),
                        ttype: TokenType::from(stream_item),
                    })
                }
            }
        }

        None
    }
}

mod tests {
    use crate::Lexer;

    #[test]
    fn read_stream_of_tokens() {
        let tokens_as_str = String::from("12 + 1");
        let lexer = Lexer::from(tokens_as_str);

        for v in lexer.into_iter() {
            println!(">{v:?}");
        }
    }
}
