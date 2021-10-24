use crate::ast;
use crate::errors;
use std::collections::HashMap;

pub type Result<T, E = errors::TypingError> = std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    pub environment_traits: HashMap<String, ast::TraitTypeDeclaration>,
}

fn create_builtins() -> HashMap<String, ast::TraitTypeDeclaration> {
    let mut result = HashMap::<String, ast::TraitTypeDeclaration>::new();
    return result;
}

fn compare_struct_trait(
    struct_: &ast::TypeUsage,
    trait_: &ast::TypeUsage,
    struct_name: &ast::Identifier,
    trait_name: &ast::Identifier,
) -> Result<()> {
    match struct_ {
        ast::TypeUsage::Named(named) => match trait_ {
            ast::TypeUsage::Named(trait_named) => {
                if named.name.name.value == trait_named.name.name.value
                    || (named.name.name.value == struct_name.name.value && trait_named.name.name.value == trait_name.name.value)
                {
                    return Ok(());
                }
                return Err(errors::TypingError::TypeMismatch {
                    type_one: struct_.clone(),
                    type_two: trait_.clone(),
                });
            }
            ast::TypeUsage::Function(_) => {
                return Err(errors::TypingError::TypeMismatch {
                    type_one: struct_.clone(),
                    type_two: trait_.clone(),
                });
            }
            _ => panic!("Unknown in function definition"),
        },
        ast::TypeUsage::Function(function) => match trait_ {
            ast::TypeUsage::Named(_) => {
                return Err(errors::TypingError::TypeMismatch {
                    type_one: struct_.clone(),
                    type_two: trait_.clone(),
                });
            }
            ast::TypeUsage::Function(trait_function) => {
                if function.arguments.len() != trait_function.arguments.len() {
                    return Err(errors::TypingError::TypeMismatch {
                        type_one: struct_.clone(),
                        type_two: trait_.clone(),
                    });
                }
                for (i, _) in function.arguments.iter().enumerate() {
                    compare_struct_trait(&function.arguments[i], &trait_function.arguments[i], struct_name, trait_name)?;
                }
                compare_struct_trait(&function.return_type, &trait_function.return_type, struct_name, trait_name)?;
                return Ok(());
            }
            _ => panic!("Unknown in function definition"),
        },
        _ => panic!("Unknown in function definition"),
    }
}

pub struct TraitChecker {}

impl TraitChecker {
    pub fn with_module(self: &Self, module: &ast::Module) -> Result<()> {
        let mut ctx = Context {
            environment_traits: create_builtins(),
        };

        for item in module.items.iter() {
            match item {
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Trait(trait_)) => {
                    ctx.environment_traits.insert(trait_.name.name.value.to_string(), trait_.clone());
                }
                _ => {}
            }
        }

        for item in module.items.iter() {
            match item {
                ast::ModuleItem::Impl(impl_) => {
                    self.with_impl(&ctx, impl_)?;
                }
                _ => {}
            }
        }
        return Ok(());
    }

    fn with_impl(self: &Self, ctx: &Context, impl_: &ast::Impl) -> Result<()> {
        // See if trait actually matches
        match &impl_.trait_ {
            Some(trait_) => {
                // assert trait functions satisfied
                if !ctx.environment_traits.contains_key(&trait_.name.value) {
                    return Err(errors::TypingError::TypeDoesNotExist {
                        identifier: trait_.clone(),
                    });
                }
                let trait_declaration = &ctx.environment_traits[&trait_.name.value];
                for trait_item in trait_declaration.functions.iter() {
                    match trait_item {
                        ast::TraitItem::FunctionDeclaration(declaration) => {
                            let mut found = false;
                            for impl_function in impl_.functions.iter() {
                                if impl_function.declaration.name.name.value == declaration.name.name.value {
                                    found = true;
                                    compare_struct_trait(
                                        &impl_function.declaration.to_type(),
                                        &declaration.to_type(),
                                        &impl_.struct_name,
                                        &trait_,
                                    )?;
                                }
                            }
                            if found == false {
                                return Err(errors::TypingError::MissingTraitFunction {
                                    struct_name: impl_.struct_name.clone(),
                                    function_name: declaration.name.clone(),
                                });
                            }
                        }
                        ast::TraitItem::Function(function) => {
                            // skip found check because it has a default
                            for impl_function in impl_.functions.iter() {
                                if impl_function.declaration.name.name.value == function.declaration.name.name.value {
                                    compare_struct_trait(
                                        &impl_function.declaration.to_type(),
                                        &function.declaration.to_type(),
                                        &impl_.struct_name,
                                        &trait_,
                                    )?;
                                }
                            }
                        }
                    }
                }
                // assert all functions are in trait
                for impl_function in impl_.functions.iter() {
                    let mut found = false;
                    for trait_item in trait_declaration.functions.iter() {
                        let declaration = match trait_item {
                            ast::TraitItem::Function(function) => &function.declaration,
                            ast::TraitItem::FunctionDeclaration(declaration) => &declaration,
                        };
                        if impl_function.declaration.name.name.value == declaration.name.name.value {
                            found = true;
                            break;
                        }
                    }
                    if found == false {
                        return Err(errors::TypingError::FunctionNotInTrait {
                            function_name: impl_function.declaration.name.clone(),
                        });
                    }
                }
            }
            None => {}
        }
        // TODO: check for duplicate functions
        return Ok(());
    }
}
