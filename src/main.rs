mod ast;
#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

mod compiler;
use inkwell::context::Context;

fn main() {
    let module_ast = grammar::ModuleParser::new().parse("
    fn add(a, b) {
        a + b
    }
    fn subtract(a, b) {
        a - b
    }
    fn main() {
        add(4, subtract(5, 2))
    }
    ").unwrap();

    let context = Context::create();
    let mut code_gen = compiler::ModuleCodeGen::new(&context, "main".to_string());
    code_gen.gen_module(module_ast);
    println!("{}", code_gen.dump());
}


#[test]
fn grammar() {
    assert!(grammar::LiteralIntParser::new().parse("22").is_ok());
    assert!(grammar::IdentifierParser::new().parse("foo").is_ok());
    assert!(grammar::LiteralIntParser::new().parse("2a").is_err());

    assert!(grammar::TermParser::new().parse("22").is_ok());
    assert!(grammar::TermParser::new().parse("foo").is_ok());

    assert!(grammar::ExpressionParser::new().parse("22 * foo").is_ok());
    assert!(grammar::ExpressionParser::new().parse("22 * 33").is_ok());
    assert!(grammar::ExpressionParser::new().parse("(22 * 33) + 24").is_ok());

    assert!(grammar::BlockParser::new().parse("{ (22 * 33) + 24 }").is_ok());
    assert!(grammar::BlockParser::new().parse("{ }").is_err());

    assert!(grammar::VariableDeclarationParser::new().parse("foo").is_ok());
    assert!(grammar::VariableDeclarationParser::new().parse("1234").is_err());

    assert!(grammar::FunctionParser::new().parse("fn add(a, b) { a + b }").is_ok());
    assert!(grammar::FunctionParser::new().parse("fn random_dice_roll() { 4 }").is_ok());
    assert!(grammar::FunctionParser::new().parse("fn add(a, b) { a + }").is_err());
    assert!(grammar::FunctionParser::new().parse("fn add(a, b)").is_err());

    assert!(grammar::FunctionCallParser::new().parse("foo(1, 2)").is_ok());

    assert!(grammar::ModuleParser::new().parse("fn add(a, b) { a + b }").is_ok());
    assert!(grammar::ModuleParser::new().parse("fn add(a, b) { a + b } fn subtract(a, b) { a - b }").is_ok());
}
