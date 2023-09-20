use helium_lexer::token::{NumericType, Token, TokenType};

#[derive(Debug, PartialEq)]
pub enum Operator {
    Minus,
    Plus,
    Slash,
    Star,
    Bang,
    Carot,
}

impl Operator {
    pub fn infix_biding_power(&self) -> Result<(u8, u8), String> {
        match self {
            Operator::Plus | Operator::Minus => Ok((1, 2)),
            Operator::Slash | Operator::Star => Ok((3, 4)),
            _ => Err(format!(
                "operator {:?} does not have infix properties",
                self
            )),
        }
    }

    pub fn prefix_biding_power(&self) -> Result<((), u8), String> {
        match self {
            Operator::Minus => Ok(((), 5)),
            _ => Err(format!(
                "operator {:?} does not have prefix properties",
                self
            )),
        }
    }
}

impl TryFrom<&Token> for Operator {
    type Error = String;
    fn try_from(value: &Token) -> Result<Self, Self::Error> {
        match value.ttype {
            TokenType::Minus => Ok(Operator::Minus),
            TokenType::Plus => Ok(Operator::Plus),
            TokenType::Slash => Ok(Operator::Slash),
            TokenType::Star => Ok(Operator::Star),
            _ => Err(format!(
                "operator not defined: '{}' ({:?})",
                value.lexeme, value.ttype
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Unary {
    Operator(Operator),
    Expression(Box<AST>),
}

#[derive(Debug, PartialEq)]
pub enum AST {
    Integer(i32),
    Float(f32),
    Identifier(String),

    LetStatment {
        variable: String,
        rhs: Box<AST>,
    },

    UnaryExpression {
        operator: Operator,
        expression: Box<AST>,
    },

    BinaryExpression {
        operator: Operator,
        lhs: Box<AST>,
        rhs: Box<AST>,
    },

    FunctionLiteral {
        name: String,
        args: Vec<String>,
        body: Vec<Box<AST>>,
    },

    ReturnStatement(Box<AST>),
}

impl From<(NumericType, String)> for AST {
    fn from(value: (NumericType, String)) -> Self {
        match value.0 {
            NumericType::Integer => AST::Integer(value.1.parse::<i32>().unwrap()),
            NumericType::Float => AST::Float(value.1.parse::<f32>().unwrap()),
        }
    }
}
