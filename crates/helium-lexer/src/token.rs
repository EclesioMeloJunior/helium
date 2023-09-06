#[derive(Debug)]
pub enum NumericType {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub enum TokenType {
    Identifier,
    Number(NumericType),
    Plus,
}

impl From<char> for TokenType {
    fn from(value: char) -> Self {
        match value {
            _ if value >= '0' && value <= '9' => TokenType::Number(NumericType::I32),
            _ if value == '+' => TokenType::Plus,
            _ => TokenType::Identifier,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub lexeme: String,
    pub ttype: TokenType,
}
