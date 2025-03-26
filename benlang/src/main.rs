use anyhow::Result;
use cfg::CFGBuilder;
use codegen::Generator;
use parser::Parser;
use parser::scanner::Scanner;

fn main() -> Result<()> {
    let mut cfg_builder = build_cfg();
    cfg_builder.build_cfgs()?;

    let mut generator = Generator::new_from_id(&cfg_builder, cfg_builder.func_data.main);

    generator.generate_all_func_protos(&cfg_builder);

    Ok(())
}

pub fn prep_parser_cfg() -> Parser {
    static SOURCE: &str = "
            func test_func(first_param, second_param) {
                    var test_var = 0;
                    while (test_var < 100 ) {
                        if (test_var > 100) {
                            test_var = test_var + 10;
                        } else {
                            test_var = test_var + 9;
                        }
                    }
                    test_var + 3000;
                    var new_var = test_var + 1;
            }
            test_func(1, 2);
            ";
    let mut scanner = Scanner::new(SOURCE);
    scanner.scan();

    Parser::new(scanner.tokens, scanner.interner)
}
pub fn build_cfg() -> CFGBuilder {
    let mut parser = prep_parser_cfg();
    parser.build_ast().unwrap();
    let ast = parser.ast;
    let main = parser.func_data.main;
    let func_data = parser.func_data;
    let func_pool = parser.func_pool;

    CFGBuilder::new(parser.interner, main, func_data, func_pool)
}
pub fn test() {}
