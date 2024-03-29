use crate::ast;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum TypingError {
    #[error("unknown named type")]
    TypeDoesNotExist { identifier: ast::Identifier },
    #[error("identifier is not type")]
    IdentifierIsNotType { identifier: ast::Identifier },
    #[error("argument length mismatch")]
    ArgumentLengthMismatch {
        // TODO: add position
    },
    #[error("type mismatch")]
    TypeMismatch {
        type_one: ast::TypeUsage,
        type_two: ast::TypeUsage,
    },
    #[error("unknown field name")]
    UnknownFieldName { identifier: ast::Identifier },
    #[error("cannot assign to method")]
    CannotAssignToMethod { identifier: ast::Identifier },
    #[error("multiple field name matches")]
    MultipleFieldName { identifier: ast::Identifier },
    #[error("attribute gotten of non-struct")]
    AttributeOfNonstruct { identifier: ast::Identifier },
    #[error("name is not a struct, cannot instaniate")]
    NotAStructLiteral { identifier: ast::Identifier },
    #[error("struct literal fields mismatch")]
    StructLiteralFieldsMismatch { struct_name: ast::Identifier },
    #[error("missing trait function")]
    MissingTraitFunction {
        struct_name: ast::Identifier,
        function_name: ast::Identifier,
    },
    #[error("function not in trait")]
    FunctionNotInTrait { function_name: ast::Identifier },
    #[error("impl trait must be trait")]
    ImplTraitMustBeTrait { trait_name: ast::Identifier },
    #[error("function call used with non-function")]
    FunctionCallNotAFunction {
        // TODO: add position
    },
    #[error("`if` condition must be bool")]
    IfConditionMustBeBool {
        // TODO: add position
    },
    #[error("cannot use type as an expression")]
    TypeIsNotAnExpression { type_name: ast::Identifier },
    #[error("wrong number of type parameters")]
    WrongNumberOfTypeParameters {
        // TODO: add position
    },
    #[error("invalid use of alias")]
    InvalidUseofAlias,
    #[error("alias cannot have type parameters")]
    InvalidTypeParameterOnAlias {
        alias: ast::Identifier,
    },
    #[error("type cannot be used for generic")]
    InvalidTypeForGeneric,
    #[error("multiple errors")]
    MultipleErrors { errors: Vec<TypingError> },
}
