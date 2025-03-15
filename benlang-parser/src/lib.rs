mod expr;
mod expr_parser;
mod object;
pub mod scanner;
mod stmt;
mod stmt_parser;

use crate::expr::*;
use crate::expr_parser::*;
use crate::object::Object;
use crate::scanner::Symbol;
use crate::scanner::{SymbolTable, Token};
use crate::stmt::*;
use crate::stmt_parser::*;
use anyhow::{Error, Result, anyhow, ensure};
use slotmap::SlotMap;
use thiserror::Error;

pub type AST = Vec<StmtId>;

#[derive(Default, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    #[default]
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected token (expected {expected} found {found})")]
    UnexpectedToken { expected: Token, found: Token },

    #[error("cannot get prefix: {prefix}")]
    InvalidPrefix { prefix: Token },
    #[error("cannot get infix: {infix}")]
    InvalidInfix { infix: Token },
    #[error("invalid primary: {primary}")]
    InvalidPrimary { primary: Token },
    #[error("invalid operator: {op}")]
    InvalidOp { op: Token },

    #[error("unexpected end of file")]
    EOF,
    #[error("unknown error")]
    Unknown,
    #[error("todo error")]
    TODO,
}

impl TryFrom<u32> for Precedence {
    type Error = Error;
    fn try_from(value: u32) -> Result<Self, Error> {
        Ok(match value {
            0 => Precedence::None,
            1 => Precedence::Assignment,
            2 => Precedence::Or,
            3 => Precedence::And,
            4 => Precedence::Equality,
            5 => Precedence::Comparison,
            6 => Precedence::Term,
            7 => Precedence::Factor,
            8 => Precedence::Unary,
            9 => Precedence::Call,
            10 => Precedence::Primary,
            _ => return Err(anyhow!("{value} is not a value between 0-10")),
        })
    }
}

impl From<Token> for Precedence {
    fn from(val: Token) -> Self {
        match val {
            Token::StringLiteral(_) => Precedence::Primary,
            Token::Number(_) => Precedence::Primary,
            Token::Double(_) => Precedence::Primary,
            Token::Plus => Precedence::Term,
            Token::Minus => Precedence::Term,
            Token::Slash => Precedence::Factor,
            Token::Star => Precedence::Factor,
            Token::BangEqual => Precedence::Equality,
            Token::Greater => Precedence::Comparison,
            Token::GreaterEqual => Precedence::Comparison,
            Token::Less => Precedence::Comparison,
            Token::LessEqual => Precedence::Comparison,
            Token::EqualEqual => Precedence::Equality,
            Token::Return => Precedence::None,
            Token::And => Precedence::And,
            Token::LeftParen => Precedence::Call,
            Token::Dot => Precedence::Call,
            Token::Or => Precedence::Or,
            _ => Precedence::None,
        }
    }
}

impl Token {
    pub fn prec(self) -> Precedence {
        Precedence::from(self)
    }
}

#[derive(Debug, Clone)]
struct TokenStream(std::vec::IntoIter<Token>);

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Self::Item>;
cargo clippy --fix --lib -p benlang-parser
    fn into_iter(self) -> Self::IntoIter {
        self.0
    }
}

impl Iterator for ParserIter {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.prev = self.current;
        if let Some(next) = self.iter.0.next() {
            if next == Token::EOF {
                return None;
            }
            self.current = next;
            return Some(next);
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct ParserIter {
    iter: TokenStream,
    prev: Token,
    current: Token,
}

#[derive(Debug)]
pub struct Parser {
    iter: ParserIter,
    ast: AST,
    stmt_pool: SlotMap<StmtId, Stmt>,
    expr_pool: SlotMap<ExprId, Expr>,
    interner: SymbolTable,
}

impl ParserIter {
    fn new(tokens: Vec<Token>) -> ParserIter {
        ParserIter {
            iter: TokenStream(tokens.into_iter()),
            prev: Token::Invalid,
            current: Token::Invalid,
        }
    }

    fn prev_prec(&self) -> Precedence {
        self.prev.prec()
    }

    fn current_prec(&self) -> Precedence {
        self.current.prec()
    }

    fn unexpected_current_token(&self, expected: Token) -> ParseError {
        ParseError::UnexpectedToken {
            expected,
            found: self.current,
        }
    }

    fn next(&mut self) -> Result<Token> {
        if let Some(token) = self.iter.0.next() {
            self.prev = self.current;
            self.current = token;
            return Ok(self.current);
        }
        Err(Error::new(ParseError::EOF))
    }

    fn advance(&mut self) -> Result<Token> {
        self.is_at_end()?;
        self.next()?;
        Ok(self.prev)
    }

    fn consume(&mut self, to_consume: Token) -> Result<Token> {
        self.check(to_consume)?;
        self.advance()
    }

    fn check(&mut self, expected: Token) -> Result<()> {
        self.is_at_end()?;
        ensure!(
            self.current == expected,
            self.unexpected_current_token(expected)
        );
        Ok(())
    }

    pub fn is_at_end(&self) -> Result<()> {
        ensure!(self.current != Token::EOF, ParseError::EOF);
        Ok(())
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>, interner: SymbolTable) -> Self {
        let ast: AST = Vec::new();
        let mut parser = Self {
            iter: ParserIter::new(tokens),
            ast,
            stmt_pool: SlotMap::with_key(),
            expr_pool: SlotMap::with_key(),
            interner,
        };
        // 'prime' parser
        let _ = parser.advance();
        parser
    }

    fn consume(&mut self, token_type: Token) -> Result<Token> {
        self.iter.consume(token_type)
    }

    fn is_at_end(&self) -> bool {
        self.iter.current == Token::EOF
    }
    fn advance(&mut self) -> Result<Token> {
        self.iter.advance()
    }

    fn current_prec(&self) -> Precedence {
        self.iter.current_prec()
    }

    fn insert_stmt(&mut self, stmt: Stmt) -> Result<StmtId> {
        //        print!("inserting stmt: {:?}", &stmt);
        let key = self.stmt_pool.insert(stmt);
        //        println!(" // key: {key:?}");
        Ok(key)
    }

    fn insert_expr(&mut self, expr: Expr) -> Result<ExprId> {
        //        print!("inserting expr: {:?}", &expr);
        let key = self.expr_pool.insert(expr);
        //       println!(" // key: {key:?}");
        Ok(key)
    }

    fn parse_var(&mut self) -> Result<Symbol> {
        if let Token::Identifier(iden) = self.iter.current {
            self.advance()?;
            return Ok(iden);
        }
        Err(anyhow!(
            "could not parse identifier from {}",
            self.iter.current
        ))
    }

    fn check(&mut self, token: Token) -> Result<()> {
        self.iter.check(token)
    }

    pub fn get_bin(&self, expr_key: ExprId) -> Result<(&Expr, BinaryOp, &Expr)> {
        if let Some(Expr::Binary(bin)) = self.expr_pool.get(expr_key) {
            let lhs = bin.0;
            let rhs = bin.2;
            let op = bin.1;
            return Ok((
                self.expr_pool.get(lhs).unwrap(),
                op,
                self.expr_pool.get(rhs).unwrap(),
            ));
        }
        Err(anyhow!("could not get bin {:?}", expr_key))
    }

    pub fn build_ast(&mut self) -> Result<&[StmtId]> {
        while !self.is_at_end() {
            let key = self.declaration()?;
            self.ast.push(key);
        }
        //        for func in self.stmt_pool.funcs.0.values() {
        //            self.resolver.unroll_block(func.body);
        //        }
        Ok(&self.ast)
    }
}

#[cfg(test)]
mod parser_tests {

    use super::*;
    use crate::scanner::Scanner;

    #[test]

    fn parse_func() {
        static SOURCE: &str = "
            func test_func(first_param, second_param) {    
                    if (true == true) {
                    var test_var = 1 + 1;cargo clippy --fix --lib -p benlang-parser
                    test_var + 1;
                } else {
                    {
                    while (true) {
                        2 + 2;
                        3 + 3;
                        4 + 4;
                        1 + 1;
                    }}
                }
            }";
        let mut scanner = Scanner::new(SOURCE);
        scanner.scan();

        let mut parser = Parser::new(scanner.tokens, scanner.interner);
        if let Ok(ast) = parser.build_ast() {
          //TODO
        }
    }
}
