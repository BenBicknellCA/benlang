use anyhow::Result;
use cfg::CFGBuilder;
use codegen::Generator;
use parser::Parser;
use parser::scanner::Scanner;

fn main() -> Result<()> {
    let mut cfg = build_cfg();
    cfg.build_cfgs()?;
    //    for cfg_built in cfg.func_to_cfg {}

    Ok(())
}

pub fn prep_parser_cfg() -> Parser {
    static SOURCE: &str = "
            func test_func(first_param, second_param) {
                    var test_var = 0;
                    while (true && true) {
                        if (false) {
                            test_var = 11223 * 99;
                        } else {
                            test_var = 100 - 22;
                        }
                    }
                    test_var + 3000;
                    var new_var = test_var + 1;
            }

            func second_test_func() {
                1 + 2 + 3;
            }
            ";
    let mut scanner = Scanner::new(SOURCE);
    scanner.scan();

    let mut parser = Parser::new(scanner.tokens, scanner.interner);
    parser
}
pub fn build_cfg() -> CFGBuilder {
    let mut parser = prep_parser_cfg();
    parser.build_ast().unwrap();
    let ast = parser.ast;
    let main = parser.func_data.main;
    let func_data = parser.func_data;
    let func_pool = parser.func_pool;

    let cfg_builder = CFGBuilder::new(parser.interner, main, func_data, func_pool);
    cfg_builder
}
pub fn test() {}
