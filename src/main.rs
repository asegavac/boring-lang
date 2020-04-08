mod ast;
#[macro_use] extern crate lalrpop_util;

lalrpop_mod!(pub grammar); // synthesized by LALRPOP

fn main() {
    println!("Hello, world!");
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

    assert!(grammar::ProgramParser::new().parse("fn add(a, b) { a + b }").is_ok());
    assert!(grammar::ProgramParser::new().parse("fn add(a, b) { a + b } fn subtract(a, b) { a - b }").is_ok());
}
