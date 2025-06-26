pub mod expr;
pub mod expr_parser;
pub mod object;
pub mod scanner;
pub mod stmt;
pub mod stmt_parser;
pub mod value;

use crate::expr::*;
use crate::expr_parser::*;
use crate::object::{FuncId, Function, Variables};
use crate::scanner::Symbol;
use crate::scanner::{SymbolTable, Token};
use crate::stmt::*;
use crate::stmt_parser::*;
use anyhow::{Error, Result, anyhow, ensure};
use slotmap::{SecondaryMap, SlotMap};
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
            //            Token::Double(_) => Precedence::Primary,
            Token::Plus => Precedence::Term,
            Token::Minus => Precedence::Term,
            Token::Slash => Precedence::Factor,
            Token::Star => Precedence::Factor,
            Token::Mod => Precedence::Factor,
            Token::BangEqual => Precedence::Equality,
            Token::Greater => Precedence::Comparison,
            Token::GreaterEqual => Precedence::Comparison,
            Token::Less => Precedence::Comparison,
            Token::LessEqual => Precedence::Comparison,
            Token::EqualEqual => Precedence::Equality,
            Token::Return => Precedence::None,
            Token::And => Precedence::And,

            //not sure about bitwise prec
            Token::BitAnd => Precedence::And,
            Token::BitOr => Precedence::Or,

            Token::LeftParen => Precedence::Call,
            Token::Dot => Precedence::Call,
            Token::Or => Precedence::Or,
            Token::Equal => Precedence::None,
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

pub type StmtPool = SlotMap<StmtId, Stmt>;
pub type ExprPool = SlotMap<ExprId, Expr>;

pub type FuncPool = SlotMap<FuncId, Function>;

#[derive(Debug)]
pub struct Parser {
    iter: ParserIter,
    pub ast: AST,
    pub func_pool: FuncPool,
    pub func_data: FuncData,
    pub interner: SymbolTable,
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
#[derive(Debug, Default)]
pub struct FuncData {
    pub child_to_parent: SecondaryMap<FuncId, FuncId>,
    pub parent_to_children: SecondaryMap<FuncId, Vec<FuncId>>,
    pub main: FuncId,
    pub current: FuncId,
    pub stmt_pools: SecondaryMap<FuncId, StmtPool>,
    pub expr_pools: SecondaryMap<FuncId, ExprPool>,
    pub variables: SecondaryMap<FuncId, Variables>,
}

impl FuncData {
    pub fn new_main(func_pool: &mut FuncPool) -> Self {
        let main = func_pool.insert(Function::default());
        func_pool[main].func_id = main;
        let current = main;
        let mut parent_to_children = SecondaryMap::new();

        let mut expr_pools: SecondaryMap<FuncId, ExprPool> = SecondaryMap::new();
        let mut stmt_pools: SecondaryMap<FuncId, StmtPool> = SecondaryMap::new();

        expr_pools.insert(main, ExprPool::with_key());
        stmt_pools.insert(main, StmtPool::with_key());
        parent_to_children.insert(main, Vec::new());
        let mut variables = SecondaryMap::new();
        variables.insert(main, Variables::new());

        Self {
            main,
            current,
            child_to_parent: SecondaryMap::new(),
            parent_to_children,
            expr_pools,
            stmt_pools,
            variables,
        }
    }
}

impl Parser {
    pub fn new(tokens: Vec<Token>, interner: SymbolTable) -> Self {
        let ast: AST = Vec::new();
        let mut func_pool: FuncPool = FuncPool::with_key();
        let func_data = FuncData::new_main(&mut func_pool);
        let mut parser = Self {
            iter: ParserIter::new(tokens),
            ast,
            func_data,
            func_pool,

            interner,
        };
        // 'prime' parser
        parser.advance().unwrap();
        parser
    }

    pub fn current_func(&self) -> FuncId {
        self.func_data.current
    }

    pub fn insert_expr_in_func_pool(&mut self, func: FuncId, expr: Expr) -> ExprId {
        self.func_data.expr_pools[func].insert(expr)
    }

    pub fn insert_expr_in_current_func(&mut self, expr: Expr) -> ExprId {
        let current = self.current_func();
        self.insert_expr_in_func_pool(current, expr)
    }

    pub fn insert_stmt_in_func_pool(&mut self, func: FuncId, stmt: Stmt) -> StmtId {
        self.func_data.stmt_pools[func].insert(stmt)
    }

    pub fn insert_stmt_in_current_func(&mut self, stmt: Stmt) -> StmtId {
        let current = self.current_func();
        self.insert_stmt_in_func_pool(current, stmt)
    }

    pub fn insert_stmt_in_current_func_body(&mut self, stmt: StmtId) {
        let current = self.current_func();
        self.func_pool[current].body.body.push(stmt);
    }

    pub fn enter_func(&mut self, parent: FuncId) -> FuncId {
        let placeholder_child = self.func_pool.insert(Function::default());
        self.func_data.current = placeholder_child;
        self.func_pool[placeholder_child].func_id = placeholder_child;

        self.func_data
            .stmt_pools
            .insert(placeholder_child, SlotMap::with_key());

        self.func_data
            .expr_pools
            .insert(placeholder_child, SlotMap::with_key());

        self.func_data
            .child_to_parent
            .insert(placeholder_child, parent);

        if let Some(children) = self.func_data.parent_to_children.get_mut(parent) {
            children.push(placeholder_child);
        } else {
            self.func_data
                .parent_to_children
                .insert(parent, vec![placeholder_child]);
        }
        placeholder_child
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
        let key = self.insert_stmt_in_func_pool(self.current_func(), stmt);
        //        println!(" // key: {key:?}");
        Ok(key)
    }

    fn insert_expr(&mut self, expr: Expr) -> Result<ExprId> {
        let key = self.insert_expr_in_func_pool(self.current_func(), expr);
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
        if let Some(Expr::Binary(bin)) =
            self.func_data.expr_pools[self.current_func()].get(expr_key)
        {
            let lhs = bin.lhs;
            let rhs = bin.rhs;
            let op = bin.op;
            return Ok((
                self.func_data.expr_pools[self.current_func()]
                    .get(lhs)
                    .unwrap(),
                op,
                self.func_data.expr_pools[self.current_func()]
                    .get(rhs)
                    .unwrap(),
            ));
        }
        Err(anyhow!("could not get bin {:?}", expr_key))
    }

    pub fn build_ast(&mut self) -> Result<()> {
        while !self.is_at_end() {
            let key = self.declaration()?;
            self.func_pool[self.func_data.current].body.body.push(key);
            //            for (func_id, val) in &self.func_data.stmt_pools {
            //                if self.func_data.main != func_id {
            //                    for val in val.values() {
            //                        println!("val: {val:?}");
            //
            //                        //                     println!("id: {:?} val: {:?}", self.func_data.stmt_pools[func_id][id], val);
            //                        //                     if let Some(Stmt::Expr(expr)) = self.func_data.stmt_pools[self.func_data.main].get(id) {
            //                        if let Stmt::Expr(expr) = val {
            //                            println!("expr: {:?}", &self.func_data.expr_pools[func_id][*expr]);
            //                        }
            //                    }
            //                }
            //                //            self.ast.push(key);
            //            }
        }
        Ok(())
    }
    //        //       Ok(&self.ast)
}

#[cfg(test)]
pub(crate) mod parser_tests {
    use super::*;
    use crate::scanner::Scanner;

    pub fn prep_parser_tests(source: &'static str) -> Parser {
        let mut scanner = Scanner::new(source);
        scanner.scan();

        
        Parser::new(scanner.tokens, scanner.interner)
    }

    #[test]
    pub fn parse_func() {
        static SOURCE: &str = "
            func test_func(first_param, second_param) {    
                    if (true == true) {
                    var test_var = 1 + 1;
                    test_var + 1;
                    test_var;
                } else {
                    {
                    while (true) {
                        test_var < 1;
                        3 + 3;
                        4 + 4;
                        1 + 1;
                    }}
                }
            }";

        let mut parser = prep_parser_tests(SOURCE);
        assert!(parser.build_ast().is_ok());
    }

    fn parse_block() {
        static SOURCE: &str = "{}";
        let parser = prep_parser_tests(SOURCE);
        // TODO
    }
}
