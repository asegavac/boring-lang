

pub enum Operator {
    Mul,
    Div,
    Plus,
    Minus,
    // Gt,
    // Gte,
    // Lt,
    // Lte,
    // Eq,
    // Mod,
    // Exp,
    // FloorDiv,
}


pub struct LiteralInt {
    pub value: i64
}

// pub struct LiteralString {
//     value: String
// }

pub struct Identifier {
    pub name: String
}

pub struct FunctionCall {
    pub name: Identifier,
    pub arguments: Vec<Box<Expression>>,
}

pub enum Expression {
    LiteralInt(LiteralInt),
    // LiteralString(LiteralString),
    FunctionCall(FunctionCall),
    Identifier(Identifier),
    Op(Box<Expression>, Operator, Box<Expression>),
}

pub struct Block {
    pub expression: Box<Expression>
}

pub struct VariableDeclaration {
    pub name: Identifier,
    // type: Identifier,
}

pub struct Function {
    pub name: Identifier,
    // return_type: Identifier,
    pub arguments: Vec<VariableDeclaration>,
    pub block: Block,
}


// pub struct Assignment {
//     variable: VariableDeclaration,
//     expression: Expression,
// }


pub struct Module {
    pub functions: Vec<Function>,
}
