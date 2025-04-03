use anyhow::Result;
use cfg::CFGBuilder;
use codegen::Compiler;
use parser::Parser;
use parser::scanner::Scanner;
use vm::VM;

fn main() -> Result<()> {
    run_program()
}

pub fn run_program() -> Result<()> {
    let mut cfg_builder = build_cfg()?;
    cfg_builder.build_cfgs()?;
    let main = cfg_builder.func_data.main;
    let mut compiler = Compiler::new_from_id(&cfg_builder, cfg_builder.func_data.main);

    compiler.compile_all_funcs(&cfg_builder)?;

    let mut vm = VM::new(compiler.func_protos, cfg_builder.symbol_table, main);
    vm.run_program(true)?;

    Ok(())
}

pub fn prep_parser_cfg() -> Result<Parser> {
    let contents = include_str!("../../examples/fib");

    let mut scanner = Scanner::new(contents);
    scanner.scan();

    Ok(Parser::new(scanner.tokens, scanner.interner))
}
pub fn build_cfg() -> Result<CFGBuilder> {
    let mut parser = prep_parser_cfg()?;
    parser.build_ast()?;
    let main = parser.func_data.main;
    let func_data = parser.func_data;
    let func_pool = parser.func_pool;

    Ok(CFGBuilder::new(parser.interner, main, func_data, func_pool))
}
