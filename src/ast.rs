

pub enum Operator {
    Mul,
    Div,
    Plus,
    Minus,
    Gt,
    Gte,
    Lt,
    Lte,
    Eq,
    Mod,
    Exp,
    FloorDiv,
}


pub struct LiteralInt {
    pub value: i32
}

// pub struct LiteralString {
//     value: String
// }

pub struct Identifier {
    pub name: String
}

pub enum Expression {
    LiteralInt(LiteralInt),
    // LiteralString(LiteralString),
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


pub struct Program {
    pub functions: Vec<Function>,
}
