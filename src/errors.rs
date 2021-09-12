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
    #[error("attribute gotten of non-struct")]
    AttributeOfNonstruct { identifier: ast::Identifier },
    #[error("name is not a struct, cannot instaniate")]
    NotAStructLiteral { identifier: ast::Identifier },
    #[error("struct literal fields mismatch")]
    StructLiteralFieldsMismatch {
        struct_name: ast::Identifier,
        struct_definition_name: ast::Identifier,
    },
    #[error("function call used with non-function")]
    FunctionCallNotAFunction {
        // TODO: add position
    },
    #[error("multiple errors")]
    MultipleErrors { errors: Vec<TypingError> },
}
