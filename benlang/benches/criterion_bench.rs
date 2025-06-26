use anyhow::Result;
use cfg::CFGBuilder;
use codegen::Compiler;
use criterion::{Criterion, criterion_group, criterion_main};
use parser::Parser;
use parser::scanner::Scanner;
use std::hint::black_box;
use vm::VM;

pub fn compiler_bench(c: &mut Criterion) {
    let mut parser = prep_parser_cfg().unwrap();
    parser.build_ast().unwrap();
    let main = parser.func_data.main;
    let func_data = parser.func_data;
    let func_pool = parser.func_pool;

    let mut cfg_builder = CFGBuilder::new(parser.interner, main, func_data, func_pool);
    cfg_builder.build_cfgs().unwrap();

    let mut compiler = Compiler::new_from_id(&cfg_builder, main);

    c.bench_function("compile fib 20", |b| {
        b.iter(|| {
            compiler.compile_all_funcs(black_box(&cfg_builder)).unwrap();
            black_box(())
        })
    });
}

pub fn scanner_parser_bench(c: &mut Criterion) {
    c.bench_function("scan parse fib 20", |b| {
        b.iter(|| black_box(prep_parser_cfg()))
    });
}

pub fn vm_bench(c: &mut Criterion) {
    let mut cfg_builder = build_cfg().unwrap();
    cfg_builder.build_cfgs().unwrap();
    let main = cfg_builder.func_data.main;
    let mut compiler = Compiler::new_from_id(&cfg_builder, cfg_builder.func_data.main);

    compiler.compile_all_funcs(&cfg_builder).unwrap();

    let mut vm = VM::new(compiler.func_protos, cfg_builder.symbol_table, main);

    c.bench_function("vm-fib", |b| {
        b.iter(|| {
            vm.run_program().unwrap();
            black_box(())
        })
    });
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

criterion_group!(
    benches,
    scanner_parser_bench,
    compiler_bench,
    vm_bench,
    //    control_flow_graph_bench
);
criterion_main!(benches);
