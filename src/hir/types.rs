use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};
use inkwell::context::Context;
use crate::ast;
use crate::hir:errors;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Signedness {
    Signed,
    Unsigned,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum IntBitness {
    X8,
    X16,
    X32,
    X64,
    X128,
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FloatBitness {
    X32,
    X64,
    X128,
}


#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct IntTypeDef {
    pub signedness: Signedness,
    pub bitness: IntBitness,
}


#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FloatTypeDef {
    pub bitness: FloatBitness,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTypeDef {
    pub arguments: Vec<Type>,
    pub return_type: Type,
}


#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    Int(IntTypeDef),
    Float(FloatTypeDef),
    Function(FunctionTypeDef),
    // String(StringTypeDef),
    // Struct(StructTypeDef),
    // Trait(TraitTypeDef),
    // Void,
    // Never,
}

impl Type {
    /// Construct an inkwell value for this type. Will eventually take in a list of types for
    /// generic params.
    pub fn construct(&self, context: Context) -> BasicValueEnum {
        match self {
            Bool => context.bool_type(),
            Int(type_def) => {
                // Signed + Unsigned use the same size, builder operations determine signedness
                match type_def.bitness {
                    X8 => context.i8_type(),
                    X16 => context.i16_type(),
                    X32 => context.i32_type(),
                    X64 => context.i64_type(),
                    X128 => context.i128_type(),
                }
            },
            Float(type_def) => {
                match type_def.bitness {
                    X32 => context.f32_type(),
                    X64 => context.f64_type(),
                    X128 => context.f128_type(),
                }
            },
            Function(type_def) => {
                let return_type = type_def.return_type.construct(context);
                let mut args = Vec::new();
                for arg in &type_def.arguments {
                    args.push(arg.construct(context));
                }
                return_type.fn_type(&args, false)
            },
        }
    }
}


/// Used for places where type info may or may not be solved.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub enum SpecifiedType {
    Unknown,
    Type(Type),
}

/// Used by HIR in any place that needs a type. Allows `Unknown` types for places that haven't
/// been solved yet, and will verify/solve types via the `Compare` method. This will also
/// eventually contain info from escape analysis for if it will need to be created on the stack.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct TypeLocation {
    pub specified_type: SpecifiedType,
    // pub type_parameters: Vec<TypeLocation>, // will be used for generics
    pub span: ast::Span,
}

impl TypeLocation {
    // Checks if the types are valid to be used this way, returns true
    // if we updated an unknown.
    pub fn compare(&mut self, other: &mut Self) -> Result<bool, errors::CompilerError> {
        match self.specified_type {
            SpecifiedType::Unknown => match other.specified_type {
                SpecifiedType::Unknown => false,
                SpecifiedType::Type(ty) => {
                    self.specified_type = ty;
                    true
                },
            },
            SpecifiedType::Type(ty) => match other.specified_type {
                SpecifiedType::Unknown => {
                    other.specified_type = ty;
                    true
                },
                SpecifiedType::Type(other_ty) => {
                    if ty == other_ty {
                        false
                    } else {
                        Err(errors::CompilerError{
                            message: "types do not match",
                            parts: vec![errors::ErrorPart{
                                message: format!("type {} must match", self.specified_type),
                                label_type: errors::ErrorLabel::Primary,
                                start: self.span.left,
                                end: self.span.right,
                            }, errors::ErrorPart{
                                message: format!("type {}", other.specified_type),
                                label_type: errors::ErrorLabel::Primary,
                                start: other.span.left,
                                end: other.span.right,
                            }],
                        })
                    }
                }
            }
        }
    }
}
