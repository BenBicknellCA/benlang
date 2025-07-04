use crate::Parser;
use crate::expr::*;
use crate::expr_parser::*;

use crate::object::Function;
use crate::scanner::Symbol;
use crate::scanner::Token;
use crate::stmt::*;
use crate::value::{Literal, Value};
use anyhow::Result;
use slotmap::new_key_type;

new_key_type! {pub struct StmtId;}

impl Parser {
    fn for_statement(&mut self) -> Result<StmtId> {
        self.consume(Token::For)?;
        todo!()
    }
    fn if_statement(&mut self) -> Result<StmtId> {
        self.consume(Token::If)?;

        self.consume(Token::LeftParen)?;

        let cond = self.expression()?;

        self.consume(Token::RightParen)?;

        let then_ = self.block()?;

        let else_: Option<Block> = if self.consume(Token::Else).is_ok() {
            Some(self.block()?)
        } else {
            None
        };

        self.insert_stmt(Stmt::If(If::new(cond, then_, else_)))
    }
    fn return_statement(&mut self) -> Result<StmtId> {
        self.consume(Token::Return)?;

        if self.check(Token::Semicolon).is_ok() {
            return Ok(self.insert_stmt_in_current_func(Stmt::Return0));
        }

        let to_ret = self.expression()?;
        self.consume(Token::Semicolon)?;
        self.insert_stmt(Stmt::Return1(to_ret))
    }

    fn expr_stmt(&mut self) -> Result<StmtId> {
        let expr_key: ExprId = self.expression()?;
        self.consume(Token::Semicolon)?;
        self.insert_stmt(Stmt::Expr(expr_key))
    }
    fn function(&mut self) -> Result<StmtId> {
        let parent = self.func_data.current;
        let child = self.enter_func(parent);

        self.consume(Token::Func)?;
        let name: Symbol = self.parse_var()?;
        self.consume(Token::LeftParen)?;
        let mut arity: u8 = 0;

        let mut params: Vec<Symbol> = Vec::new();

        if self.check(Token::RightParen).is_err() {
            params.push(self.parse_var()?);
            arity += 1;
            while self.check(Token::RightParen).is_err() {
                self.consume(Token::Comma)?;
                params.push(self.parse_var()?);
                arity += 1;
            }
        }

        self.consume(Token::RightParen)?;

        let body = self.block()?;

        // exit function
        self.func_pool[child] = Function::new(Some(name), body, arity, params, child);
        self.func_data.current = parent;

        let val = self.func_data.expr_pools[parent]
            .insert(Expr::Value(Value::Literal(Literal::Function(child))));

        let fun: StmtId = self.func_data.stmt_pools[parent].insert(Stmt::Expr(val));

        //        let fun_obj = self.resolver.insert_obj(Object::Function(fun_obj));
        Ok(fun)
        //        self.insert_stmt(Stmt::Function(fun))
    }
    fn var_declaration(&mut self) -> Result<StmtId> {
        self.advance()?;
        let var_name = self.parse_var()?;

        let var_val: ExprId = if self.consume(Token::Equal).is_ok() {
            self.expression()?
        } else {
            let current = self.current_func();
            self.func_data.expr_pools[current].insert(Value::Literal(Literal::Nil).into())
        };

        self.consume(Token::Semicolon)?;
        self.insert_stmt(Stmt::Var(Assign::new(var_name, var_val)))
    }

    fn while_statement(&mut self) -> Result<StmtId> {
        self.consume(Token::While)?;
        let cond = self.expression()?;

        let body = self.block()?;

        self.insert_stmt(Stmt::While(While::new(cond, body)))
    }

    fn block(&mut self) -> Result<Block> {
        self.consume(Token::LeftBrace)?;
        let mut block: Block = Block::new(Vec::new());

        if self.check(Token::RightBrace).is_err() {
            while self.check(Token::RightBrace).is_err() {
                let decl = self.declaration()?;
                if let Some(stmt) = self.func_data.stmt_pools[self.current_func()].get(decl)
                    && stmt.is_term()
                {
                    block.leaders.push(block.body.len() + 1);
                }
                block.add_stmt_id(decl);
            }
        };

        self.consume(Token::RightBrace)?;
        Ok(block)
    }

    fn block_stmt(&mut self) -> Result<StmtId> {
        let blck = self.block()?;
        let blck_stmt = self.insert_stmt_in_current_func(Stmt::Block(blck));
        Ok(blck_stmt)
    }

    fn print_stmt(&mut self) -> Result<StmtId> {
        self.consume(Token::Print)?;
        let to_print = self.expression()?;
        self.consume(Token::Semicolon)?;
        Ok(self.insert_stmt_in_current_func(Stmt::Print(to_print)))
    }

    fn statement(&mut self) -> Result<StmtId> {
        let stmt_key = match self.iter.current {
            Token::For => self.for_statement(),
            Token::If => self.if_statement(),
            Token::Return => self.return_statement(),
            Token::While => self.while_statement(),
            Token::LeftBrace => self.block_stmt(),
            Token::Print => self.print_stmt(),

            _ => self.expr_stmt(),
        }?;
        //        println!("stmt: {:?}", self.resolver.get_stmt(stmt_key)?);
        Ok(stmt_key)
    }

    fn class_decl(&mut self) -> Result<StmtId> {
        self.advance()?;
        todo!()
    }

    pub fn declaration(&mut self) -> Result<StmtId> {
        match self.iter.current {
            Token::Func => self.function(),
            Token::Var => self.var_declaration(),
            Token::Class => self.class_decl(),
            _ => self.statement(),
        }
    }
}
