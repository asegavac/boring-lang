// mod types;
mod ast;
mod type_alias_resolution;
#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

use std::fs;
use std::io::Write;
// mod compiler;
// use inkwell::context::Context;
extern crate clap;
use clap::{Arg, App};


fn main() {
    let matches = App::new("Boring Language Compiler")
                          .version("0.0.1")
                          .author("Andrew Segavac")
                          .about("Compiles boring language files to LLVM IR.")
                          .arg(Arg::with_name("OUTPUT")
                               .short("o")
                               .long("out")
                               .value_name("OUTOUT")
                               .help("Sets an output file")
                               .takes_value(true))
                          .arg(Arg::with_name("INPUT")
                               .help("Sets the input file")
                               .required(true)
                               .index(1))
                          .arg(Arg::with_name("v")
                               .short("v")
                               .multiple(true)
                               .help("Sets the level of verbosity"))
                          .get_matches();
    let input = matches.value_of("INPUT").unwrap();

    let default_output = input.rsplitn(2, ".").collect::<Vec<&str>>().last().unwrap().clone();
    let output = matches.value_of("OUTPUT").unwrap_or(default_output);

    let contents = fs::read_to_string(input).expect("input file not found");
    let unknown_id_gen = ast::IdGenerator::new();
    let module_ast =  grammar::ModuleParser::new().parse(&unknown_id_gen, &contents).unwrap(); //TODO: convert to error
    println!("ast: {:#?}", &module_ast);
    let alias_resolver = type_alias_resolution::TypeAliasResolver{};
    let resolved_ast = alias_resolver.with_module(&module_ast);
    println!("ast: {:#?}", &resolved_ast);


    // let context = Context::create();
    // let mut code_gen = compiler::ModuleCodeGen::new(&context, "main".to_string());
    // code_gen.gen_module(module_ast);
    //
    // let mut f = fs::File::create(output).expect("Unable to create out file");
    // f.write_all(code_gen.dump().as_bytes()).expect("Unable to write data");
}


#[test]
fn grammar() {
    let id_gen = ast::IdGenerator::new();
    assert!(grammar::LiteralIntParser::new().parse(&id_gen, "22").is_ok());
    assert!(grammar::IdentifierParser::new().parse(&id_gen, "foo").is_ok());
    assert!(grammar::LiteralIntParser::new().parse(&id_gen, "2a").is_err());

    assert!(grammar::TermParser::new().parse(&id_gen, "22").is_ok());
    assert!(grammar::TermParser::new().parse(&id_gen, "foo").is_ok());

    assert!(grammar::ExpressionParser::new().parse(&id_gen, "22 * foo").is_ok());
    assert!(grammar::ExpressionParser::new().parse(&id_gen, "22 * 33").is_ok());
    assert!(grammar::ExpressionParser::new().parse(&id_gen, "(22 * 33) + 24").is_ok());

    assert!(grammar::BlockParser::new().parse(&id_gen, "{ (22 * 33) + 24 }").is_ok());
    assert!(grammar::BlockParser::new().parse(&id_gen, "{ (22 * 33) + 24; 25 }").is_ok());
    // assert!(grammar::BlockParser::new().parse("{ (22 * 33) + 24\n 24 }").is_ok());
    assert!(grammar::BlockParser::new().parse(&id_gen, "{ }").is_ok());

    assert!(grammar::VariableDeclarationParser::new().parse(&id_gen, "foo: Int32").is_ok());
    assert!(grammar::VariableDeclarationParser::new().parse(&id_gen, "foo").is_err());
    assert!(grammar::VariableDeclarationParser::new().parse(&id_gen, "1234").is_err());

    assert!(grammar::FunctionParser::new().parse(&id_gen, "fn add(a: Int32, b: Int32): Int32 { a + b }").is_ok());
    assert!(grammar::FunctionParser::new().parse(&id_gen, "fn random_dice_roll(): Int32 { 4 }").is_ok());
    assert!(grammar::FunctionParser::new().parse(&id_gen, "fn add(a: Int32, b: Int32): Int32 { a + }").is_err());
    assert!(grammar::FunctionParser::new().parse(&id_gen, "fn add(a: Int32, b: Int32): Int32").is_err());

    assert!(grammar::FunctionCallParser::new().parse(&id_gen, "foo(1, 2)").is_ok());

    assert!(grammar::ModuleParser::new().parse(&id_gen, "fn add(a: Int32, b: Int32): Int32 { a + b }").is_ok());
    assert!(grammar::ModuleParser::new().parse(&id_gen, "fn add(a: Int32, b: Int32): Int32 { a + b } fn subtract(a: Int32, b: Int32): Int32 { a - b }").is_ok());
}
