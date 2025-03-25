use anyhow::Result;
use cfg::CFGBuilder;
use codegen::ActivationRecord;
use codegen::Generator;
use parser::Parser;
use parser::scanner::Scanner;

// symbol_table: &SymbolTable,
// cfg: CFG,
// func_data: FuncData,
// expr_pool: ExprPool,
// ssa: SSABuilder,
// func_pool: FuncPool,
// func: &Function,

// symbol_table: &SymbolTable,
// cfg: CFG,
// func_data: &FuncData,
// ssa: SSABuilder,
// func_pool: &FuncPool,
// func_id: FuncId,

fn main() -> Result<()> {
    let mut activation_records = Vec::new();
    let mut cfg_builder = build_cfg();
    cfg_builder.build_cfgs()?;
    for func in cfg_builder.func_pool.keys() {
        let cfg = &cfg_builder.func_to_cfg[func];
        let func_data = &cfg_builder.func_data;
        let expr_pool = &cfg_builder.func_pool;
        let ssa = &cfg_builder.func_to_ssa[func];
        let func_pool = &cfg_builder.func_pool;
        let func_id = func;
        let func = &cfg_builder.func_pool[func];
        let mut generate = Generator::new(
            &cfg_builder.symbol_table,
            cfg,
            func_data,
            ssa,
            &func_pool,
            func_id,
        );
        generate.generate_func_proto(func_id);
        activation_records.push(generate);
    }
    //    for record in activation_records {
    //        let record = record.func_proto;
    //        println!("{record:?}");
    //    }

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
