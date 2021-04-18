use crate::types;


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
pub struct TypeUsage {
    pub name: Spanned<Identifier>,
    pub ty: types::SpecifiedType,
    //TODO: Generics go here
    // mut, weak here
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
    pub value: i64,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionCall {
    pub name: Spanned<Identifier>,
    pub arguments: Vec<Spanned<Box<Expression>>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Operation {
    pub left: Spanned<Box<Expression>>,
    pub op: Operator,
    pub right: Spanned<Box<Expression>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Expression {
    LiteralInt(Spanned<LiteralInt>),
    FunctionCall(Spanned<FunctionCall>),
    Identifier(Spanned<Identifier>),
    Op(Operation),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Assignment {
    pub name: Spanned<Identifier>,
    pub type_usage: Option<Spanned<TypeUsage>>,
    // mut, weak here if type not used
    pub expression: Spanned<Box<Expression>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Assignment(Spanned<Assignment>),
    Expression(Spanned<Box<Expression>>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VariableDeclaration {
    pub name: Spanned<Identifier>,
    pub type_usage: Spanned<TypeUsage>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub name: Spanned<Identifier>,
    pub arguments: Vec<VariableDeclaration>,
    pub return_type: Spanned<TypeUsage>,
    pub block: Block,
}


#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub functions: Vec<Function>,
}
