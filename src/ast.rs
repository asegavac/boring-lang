use std::collections::HashMap;
use crate::types;


pub struct IdGenerator {
    counter: i64,
}

impl IdGenerator {
    pub fn new() -> Self {
        IdGenerator{counter: 0}
    }

    pub fn next(&mut self) -> String {
        self.counter += 1;
        ("S" + self.counter.to_string()).to_string()
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
    pub return_type: Box<TypeUsage>,
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

impl TypeUsage {
    pub fn new_unknown(id_gen: &mut IdGenerator) -> TypeUsage {
        return TypeUsage::Unknown(UnknownTypeUsage{
            name: id_gen.next(),
        });
    }

    pub fn new_named(identifier: &Identifier) -> TypeUsage {
        return TypeUsage::Named(NamedTypeUsage{
            name: identifier.clone(),
        });
    }

    pub fn new_builtin(name: String) -> TypeUsage {
        ast::TypeUsage::Named(ast::NamedTypeUsage{
            name: ast::Identifier{
                name: ast::Spanned{
                    span: ast::Span{left: 0, right: 0}, //todo: figure out a sane value for these
                    value: name,
                }
            }
        )
    }

    pub fn new_function(arg_count: usize, id_gen: &mut IdGenerator) -> TypeUsage {
        return TypeUsage::Function(FunctionTypeUsage{
            arguments: 0..arg_count.map(|_| => TypeUsage.new_unknown(&mut id_gen)).collect(),
            return_type: Box::new(TypeUsage.new_unknown(&mut id_gen)),
        });
    }
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
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralFloat {
    pub value: Spanned<f64>,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LiteralStruct {
    pub name: Identifier,
    pub fields: HashMap<Identifier, Expression>,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: Spanned<String>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionCall {
    pub source: Expression,
    pub arguments: Vec<Expression>,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructGetter {
    pub source: Expression,
    pub attribute: Identifier,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Operation {
    pub left: Expression,
    pub op: Operator,
    pub right: Expression,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VariableUsage {
    pub name: Identifier,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Subexpression {
    LiteralInt(LiteralInt),
    LiteralFloat(LiteralFloat),
    LiteralStruct(LiteralStruct),
    FunctionCall(FunctionCall),
    Identifier(Identifier),
    Op(Operation),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Expression {
    pub subexpression: Spanned<Box<Subexpression>>,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ReturnStatement {
    pub source: Expression,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LetStatement {
    variable_name: Identifier,
    expression: Expression,
    type_: TypeUsage,
}

pub enum AssignmentTarget {
    Variable(VariableUsage),
    StructAttr(StructGetter),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AssignmentStatement {
    pub source: AssignmentTarget,
    pub expression: Expression,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Return(ReturnStatement),
    Let(LetStatement),
    Assignment(AssignmentStatement),
    Expression(Expression),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub type_: TypeUsage,
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
pub struct StructField {
    pub name: Identifier,
    pub type_: TypeUsage,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StructTypeDeclaration {
    pub name: Identifier,
    pub fields: Vec<StructField>,
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
pub struct Impl {
    pub struct_name: Identifier,
    pub functions: Vec<Function>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ModuleItem {
    Function(Function),
    TypeDeclaration(TypeDeclaration),
    Impl(Impl),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub items: Vec<ModuleItem>,
}
