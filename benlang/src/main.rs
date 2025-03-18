use benlang_cfg::CFGBuilder;
use benlang_parser::Parser;
use benlang_parser::stmt::Stmt;
use benlang_parser::scanner::Scanner;
use benlang_parser::stmt_parser::StmtId;

use petgraph::dot::{Config, Dot};

fn main() {

    static SOURCE: &str = "
            func test_func(first_param, second_param) {
                    var test_var = 1 + 1;
                    if (true == true) {
                        test_var = 3;
                    } else {
                        test_var = 4;
                    }
                    while (true) {
                        test_var = test_var - 22000 ;
                    }
                    var new_var = test_var + 1;
            }";
    println!("SRC: {SOURCE}");
    let mut scanner = Scanner::new(SOURCE);
    scanner.scan();

    let mut parser = Parser::new(scanner.tokens, scanner.interner);

    parser.build_ast().unwrap();
    let ast = parser.ast;
    let func_id: StmtId = ast[0];
    let func = &parser.stmt_pool[func_id].clone();
    let mut cfg_builder =
        CFGBuilder::new(parser.interner, parser.stmt_pool, parser.expr_pool, func_id);


    let func = &cfg_builder.stmt_pool[cfg_builder.func_stmt_id].clone();
    if let Stmt::Function(func) = func {
        cfg_builder.build_func_cfg(func).unwrap();
        println!(
            "{:?}",
            Dot::with_config(&cfg_builder.cfg, &[Config::EdgeNoLabel])
        );
    };

        for (id, phi) in cfg_builder.ssa.phis.0.iter() {
            println!("opnds for {id:?}: {:?}", cfg_builder.ssa.phi_operands[id]);

        }






}
