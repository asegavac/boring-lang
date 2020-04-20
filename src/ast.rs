

pub enum Operator {
    Mul,
    Div,
    Plus,
    Minus,
}


pub struct LiteralInt {
    pub value: i64
}

pub struct Identifier {
    pub name: String
}

pub struct FunctionCall {
    pub name: Identifier,
    pub arguments: Vec<Box<Expression>>,
}

pub enum Expression {
    LiteralInt(LiteralInt),
    FunctionCall(FunctionCall),
    Identifier(Identifier),
    Op(Box<Expression>, Operator, Box<Expression>),
}

pub struct Block {
    pub expression: Box<Expression>
}

pub struct VariableDeclaration {
    pub name: Identifier,
}

pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<VariableDeclaration>,
    pub block: Block,
}


pub struct Module {
    pub functions: Vec<Function>,
}
