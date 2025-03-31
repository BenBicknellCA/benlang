use anyhow::Result;
use cfg::CFGBuilder;
use codegen::Compiler;
use parser::Parser;
use parser::scanner::Scanner;
use vm::VM;

fn main() -> Result<()> {
    let mut cfg_builder = build_cfg();
    for asd in cfg_builder.func_pool.values() {}
    cfg_builder.build_cfgs()?;
    let main = cfg_builder.func_data.main;
    let mut generator = Compiler::new_from_id(&cfg_builder, cfg_builder.func_data.main);

    generator.compile_all_funcs(&cfg_builder)?;

    let mut vm = VM::new(generator.func_protos, cfg_builder.symbol_table, main);
    vm.run_program();

    Ok(())
}

pub fn prep_parser_cfg() -> Parser {
    static SOURCE: &str = "
            func fib(n) {
                if (n <= 1) {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }
            fib(100);
            ";
    let mut scanner = Scanner::new(SOURCE);
    scanner.scan();

    Parser::new(scanner.tokens, scanner.interner)
}
pub fn build_cfg() -> CFGBuilder {
    let mut parser = prep_parser_cfg();
    parser.build_ast().unwrap();
    let main = parser.func_data.main;
    let func_data = parser.func_data;
    let func_pool = parser.func_pool;

    CFGBuilder::new(parser.interner, main, func_data, func_pool)
}
pub fn test() {}
