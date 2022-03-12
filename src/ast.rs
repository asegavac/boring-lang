use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IdGenerator {
    id_key: String,
    counter: RefCell<i64>,
}

impl IdGenerator {
    pub fn new(key: &str) -> Self {
        IdGenerator { id_key: key.to_string(), counter: RefCell::new(0) }
    }

    pub fn next(&self) -> String {
        *self.counter.borrow_mut() += 1;
        (self.id_key.to_owned() + &self.counter.borrow().to_string()).to_string()
    }
}

pub fn new_unit() -> TypeUsage {
    TypeUsage::Named(NamedTypeUsage {
        type_parameters: GenericUsage::Known(GenericInstantiation { parameters: vec![] }),
        name: Identifier {
            name: Spanned {
                span: Span { left: 0, right: 0 }, //todo: figure out a sane value for these
                value: "unit".to_string(),
            },
        },
    })
}

pub fn new_never() -> TypeUsage {
    TypeUsage::Named(NamedTypeUsage {
        type_parameters: GenericUsage::Known(GenericInstantiation { parameters: vec![] }),
        name: Identifier {
            name: Spanned {
                span: Span { left: 0, right: 0 }, //todo: figure out a sane value for these
                value: "!".to_string(),
            },
        },
    })
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub left: usize,
    pub right: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTypeUsage {
    pub arguments: Vec<TypeUsage>,
    pub return_type: Box<TypeUsage>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedTypeUsage {
    pub type_parameters: GenericUsage,
    pub name: Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnknownTypeUsage {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamespaceTypeUsage {
    Type(NamedTypeUsage)
    // Module
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeUsage {
    Function(FunctionTypeUsage),
    Named(NamedTypeUsage),
    Unknown(UnknownTypeUsage),
    Namespace(NamespaceTypeUsage)
}

impl TypeUsage {
    pub fn new_unknown(id_gen: &IdGenerator) -> TypeUsage {
        return TypeUsage::Unknown(UnknownTypeUsage { name: id_gen.next() });
    }

    pub fn new_named(identifier: &Identifier, generic_usage: &GenericUsage) -> TypeUsage {
        return TypeUsage::Named(NamedTypeUsage {
            type_parameters: generic_usage.clone(),
            name: identifier.clone(),
        });
    }

    pub fn new_builtin(name: String) -> TypeUsage {
        TypeUsage::Named(NamedTypeUsage {
            type_parameters: GenericUsage::Known(GenericInstantiation { parameters: vec![] }),
            name: Identifier {
                name: Spanned {
                    span: Span { left: 0, right: 0 }, //todo: figure out a sane value for these
                    value: name,
                },
            },
        })
    }

    pub fn new_function(arg_count: usize, id_gen: &IdGenerator) -> TypeUsage {
        return TypeUsage::Function(FunctionTypeUsage {
            arguments: (0..arg_count).map(|_| TypeUsage::new_unknown(&id_gen)).collect(),
            return_type: Box::new(TypeUsage::new_unknown(&id_gen)),
        });
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericParameter {
    pub name: Identifier,
    pub bounds: Vec<Identifier>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Generic {
    pub parameters: Vec<GenericParameter>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericInstantiation {
    pub parameters: Vec<TypeUsage>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericUsage {
    Known(GenericInstantiation),
    Unknown,
}

impl GenericUsage {
    pub fn new(type_parameters: &[TypeUsage]) -> Self {
        GenericUsage::Known(GenericInstantiation {
            parameters: type_parameters.iter().map(|tp| tp.clone()).collect(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operator {
    Mul,
    Div,
    Plus,
    Minus,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralInt {
    pub value: Spanned<String>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralFloat {
    pub value: Spanned<String>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralBool {
    pub value: Spanned<String>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralStruct {
    pub type_parameters: GenericUsage,
    pub name: Identifier,
    pub fields: Vec<(Identifier, Expression)>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: Spanned<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionCall {
    pub source: Expression,
    pub arguments: Vec<Expression>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructGetter {
    pub type_parameters: GenericUsage,
    pub source: Expression,
    pub attribute: Identifier,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Operation {
    pub left: Expression,
    pub op: Operator,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableUsage {
    pub name: Identifier,
    pub type_parameters: GenericUsage,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpression {
    pub condition: Expression,
    pub block: Block,
    pub else_: Option<Block>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Subexpression {
    LiteralInt(LiteralInt),
    LiteralFloat(LiteralFloat),
    LiteralBool(LiteralBool),
    LiteralStruct(LiteralStruct),
    FunctionCall(FunctionCall),
    VariableUsage(VariableUsage),
    If(IfExpression),
    StructGetter(StructGetter),
    Block(Block),
    Op(Operation),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expression {
    pub subexpression: Box<Subexpression>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnStatement {
    pub source: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetStatement {
    pub variable_name: Identifier,
    pub expression: Expression,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignmentTarget {
    Variable(VariableUsage),
    StructAttr(StructGetter),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignmentStatement {
    pub source: AssignmentTarget,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Return(ReturnStatement),
    Let(LetStatement),
    Assignment(AssignmentStatement),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableDeclaration {
    pub name: Identifier,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionDeclaration {
    pub generic: Generic,
    pub name: Identifier,
    pub arguments: Vec<VariableDeclaration>,
    pub return_type: TypeUsage,
}

impl FunctionDeclaration {
    pub fn to_type(&self) -> TypeUsage {
        TypeUsage::Function(FunctionTypeUsage {
            arguments: self.arguments.iter().map(|arg| arg.type_.clone()).collect(),
            return_type: Box::new(self.return_type.clone()),
        })
    }

    pub fn to_method_type(&self) -> TypeUsage {
        TypeUsage::Function(FunctionTypeUsage {
            arguments: self.arguments[1..self.arguments.len()]
                .iter()
                .map(|arg| arg.type_.clone())
                .collect(),
            return_type: Box::new(self.return_type.clone()),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
    pub declaration: FunctionDeclaration,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PrimitiveTypeDeclaration {
    pub name: String, // cannot be identifier as it's not declared anywhere specific, it's builtins
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: Identifier,
    pub type_: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructTypeDeclaration {
    pub generic: Generic,
    pub name: Identifier,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitItem {
    FunctionDeclaration(FunctionDeclaration),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitTypeDeclaration {
    pub generic: Generic,
    pub name: Identifier,
    pub functions: Vec<TraitItem>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AliasTypeDeclaration {
    pub name: Identifier,
    pub replaces: TypeUsage,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeDeclaration {
    Struct(StructTypeDeclaration),
    Primitive(PrimitiveTypeDeclaration),
    Alias(AliasTypeDeclaration),
    Trait(TraitTypeDeclaration),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    pub generic: Generic,
    pub struct_: NamedTypeUsage,
    pub trait_: Option<NamedTypeUsage>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModuleItem {
    Function(Function),
    TypeDeclaration(TypeDeclaration),
    Impl(Impl),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub items: Vec<ModuleItem>,
}
