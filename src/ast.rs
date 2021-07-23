use std::collections::HashMap;
use crate::types;


pub struct IdGenerator {
    counter: i64,
}

impl IdGenerator {
    pub fn new() -> Self {
        IdGenerator{counter: 0}
    }

    pub fn next(&mut self) -> i64 {
        self.counter += 1;
        self.counter
    }
}

pub fn new_named(name: String) -> {
    ast::TypeUsage::Named(ast::NamedTypeUsage{
        name: ast::Identifier{
            name: ast::Spanned{
                span: ast::Span{left: 0, right: 0}, //todo: figure out a sane value for these
                value: name,
            }
        }
    )
}

pub fn new_unit() -> {
    ast::TypeUsage::Named(ast::NamedTypeUsage{
        name: ast::Identifier{
            name: ast::Spanned{
                span: ast::Span{left: 0, right: 0}, //todo: figure out a sane value for these
                value: "()".to_string(),
            }
        }
    )
}

pub fn new_never() -> {
    ast::TypeUsage::Named(ast::NamedTypeUsage{
        name: ast::Identifier{
            name: ast::Spanned{
                span: ast::Span{left: 0, right: 0}, //todo: figure out a sane value for these
                value: "!".to_string(),
            }
        }
    )
}


#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub left: usize,
    pub right: usize
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}


#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionTypeUsage {
    pub arguments: Vec<TypeUsage>,
    pub return_type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct NamedTypeUsage {
    name: Identifier,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct UnknownTypeUsage {
    name: String,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeUsage {
    Function(FunctionTypeUsage),
    Named(NamedTypeUsage),
    Unknown(UnknownTypeUsage),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    Mul,
    Div,
    Plus,
    Minus,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralInt {
    pub value: Spanned<i64>,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralFloat {
    pub value: Spanned<f64>,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralStruct {
    pub fields: HashMap<Identifier, Expression>,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: Spanned<String>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionCall {
    pub source: Expression,
    pub arguments: Vec<Expression>,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructGetter {
    pub source: Box<Expression>,
    pub attribute: Identifier,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Operation {
    pub left: Expression,
    pub op: Operator,
    pub right: Expression,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VariableUsage {
    pub name: Identifier,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Subexpression {
    LiteralInt(LiteralInt),
    FunctionCall(FunctionCall),
    Identifier(Identifier),
    Op(Operation),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Expression {
    pub subexpression: Spanned<Box<Subexpression>>,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ReturnStatement {
    pub source: Expression,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LetStatement {
    variable_name: Identifier,
    type: VariableUsage,
    expression: Expression,
}

pub enum AssignmentTarget {
    Variable(VariableUsage),
    StructAttr(StructGetter),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AssignmentStatement {
    pub source: AssignmentTarget,
    pub expression: Expression,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Assignment(Assignment),
    Expression(Expression),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionDeclaration {
    pub name: Identifier,
    pub arguments: Vec<VariableDeclaration>,
    pub return_type: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub declaration: FunctionDeclaration,
    pub block: Block,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PrimitiveTypeDeclaration {
    pub name: String, // cannot be identifier as it's not declared anywhere specific, it's builtins
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructTypeDeclaration {
    pub name: Identifier,
    pub fields: HashMap<Identifier, TypeUsage>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AliasTypeDeclaration {
    pub name: Identifier,
    pub replaces: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeDeclaration {
    Struct(StructTypeDeclaration),
    Primitive(PrimitiveTypeDeclaration),
    Alias(AliasTypeDeclaration),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Impl {
    pub struct_name: Identifier,
    pub functions: Vec<Function>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub functions: Vec<Function>,
    pub types: Vec<TypeDeclaration>,
    pub impls: Vec<Impls>,
}
