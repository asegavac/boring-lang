use std::collections::HashMap;
use crate::types;
use crate::ast;



pub struct TypeChecker {}


impl TypeChecker {
    pub fn check_module(&self, types_defs: HashMap<String, types::Type>, module: &mut ast::Module) -> Result<bool> {
        let mut update = false;
        for function in module.functions {
            let update_here = self.check_function(&mut function)?;
            if update_here {
                update = true;
            }
        }
        return update;
    }

    pub fn check_function(&self, function: &mut ast::Function) -> Result<bool> {
        let mut update = false;
        for argument in function.arguments {
            let update_here = self.check_variable_declaration(argument)?;
            if update_here {
                update = true;
            }
        }
        let update_here = self.check_variable_declaration(function.return_type)?;
        if update_here {
            update = true;
        }
        let update_here = self.check_block(function.block)?;
        return update;
    }

    pub fn check_variable_declaration(&self, declaration: &mut ast::Spanned<ast::VariableDeclaration>) -> Result<bool> {
        return self.check_type_usage(&self, declaration.value.type_usage)?;
    }

    pub fn check_type_usage(&self, usage: &mut ast::Spanned<ast::TypeUsage>) {
        match usage.value.identifier.value.name {
            "Int8" => {
                let def = IntTypeDef{
                    signedness: types::Signedness::Signed,
                    bitness: types::IntBitness::X8,
                }
                usage.ty = types::SpecifiedType::Type(types::Type::Int(def))
            },
            "Int16" => {
                let def = IntTypeDef{
                    signedness: types::Signedness::Signed,
                    bitness: types::IntBitness::X16,
                }
                usage.ty = types::SpecifiedType::Type(types::Type::Int(def))
            },
            "Int32" => {
                let def = IntTypeDef{
                    signedness: types::Signedness::Signed,
                    bitness: types::IntBitness::X32,
                }
                usage.ty = types::SpecifiedType::Type(types::Type::Int(def))
            },
            "Int64" => {
                let def = IntTypeDef{
                    signedness: types::Signedness::Signed,
                    bitness: types::IntBitness::X32,
                }
                usage.ty = types::SpecifiedType::Type(types::Type::Int(def))
            },
        }
    }
}
