use crate::ast;
use crate::errors;
use std::collections::HashMap;
use std::rc::Rc;

pub type SubstitutionMap = HashMap<String, ast::TypeUsage>;

pub type Result<T, E = errors::TypingError> = std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructor {
    generic: ast::Generic,
    type_usage: ast::TypeUsage,
}

fn type_meets_trait_bounds(ctx: &Context, type_: &ast::TypeUsage, bounds: &Vec<ast::Identifier>) -> bool {
    for bound in bounds.iter() {
        let named = match type_ {
            ast::TypeUsage::Named(named) => named,
            ast::TypeUsage::Function(_) => {
                return false;
            },
            ast::TypeUsage::Unknown(_) => {
                return true; // unknown this pass, skip, test once known
            },
            ast::TypeUsage::Namespace(_) => {
                panic!("impossible");
            }
        };
        match &ctx.environment[&named.name.name.value] {
            NamedEntity::NamedType(named_type) => {
                let mut found = false;
                for impl_ in named_type.impls.iter() {
                    if impl_.trait_ == Some(bound.name.value.to_string()) {
                        found = true;
                        break;
                    }
                }
                if found == false {
                    return false;
                }
            },
            _ => {
                panic!("type is a variable, this should not happen");
            }
        }
    }
    return true;
}

fn replace_generic_with_concrete(replace: &ast::Identifier, with_type: &ast::TypeUsage, in_type: &ast::TypeUsage) -> ast::TypeUsage {
    match in_type {
        ast::TypeUsage::Named(named) => {
            let mut result = named.clone();
            if named.name.name.value == replace.name.value {
                return with_type.clone();
            }
            result.type_parameters = match &named.type_parameters {
                ast::GenericUsage::Known(known) => {
                    let mut param_result = vec!();
                    for param in known.parameters.iter() {
                        param_result.push(replace_generic_with_concrete(replace, with_type, param));
                    }
                    ast::GenericUsage::new(&param_result)
                },
                ast::GenericUsage::Unknown => {
                    ast::GenericUsage::Unknown
                },
            };
            return ast::TypeUsage::Named(result);
        },
        ast::TypeUsage::Function(func) => {
            let result = ast::TypeUsage::Function(ast::FunctionTypeUsage{
                arguments: func.arguments.iter().map(|arg| {
                    replace_generic_with_concrete(replace, with_type, arg)
                }).collect(),
                return_type: Box::new(replace_generic_with_concrete(replace, with_type, &func.return_type)),
            });
            return result;
        },
        _ => {
            // in_type is unknown, skip
            return in_type.clone();
        },
    };
}

impl TypeConstructor {
    fn from_declaration(declaration: &ast::FunctionDeclaration) -> TypeConstructor {
        return TypeConstructor{
            generic: declaration.generic.clone(),
            type_usage: declaration.to_type(),
        }
    }
    fn construct(&self, ctx: &Context, usage: &ast::GenericUsage) -> Result<ast::TypeUsage> {
        match usage {
            ast::GenericUsage::Known(known_usage) => {
                let mut result = self.type_usage.clone();
                if known_usage.parameters.len() != self.generic.parameters.len() {
                    return Err(errors::TypingError::WrongNumberOfTypeParameters{});
                }
                // 1. for arg in args, assert arg matches self.generic traits via impl name
                // 2. replace type usage names with arg types
                for i in 0..known_usage.parameters.len() {
                    if !type_meets_trait_bounds(ctx, &known_usage.parameters[i], &self.generic.parameters[i].bounds) {
                        return Err(errors::TypingError::InvalidTypeForGeneric);
                    }
                    result = replace_generic_with_concrete(&self.generic.parameters[i].name, &known_usage.parameters[i], &self.type_usage);
                }
                return Ok(result);
            },
            ast::GenericUsage::Unknown => {
                // generate new Unknown Types for matching
                let mut result = self.type_usage.clone();
                for param in self.generic.parameters.iter() {
                    let id = ctx.id_generator.next();
                    let unknown = ast::TypeUsage::Unknown(ast::UnknownTypeUsage{name: id});
                    result = replace_generic_with_concrete(&param.name, &unknown, &self.type_usage);
                }
                return Ok(result);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvImpl {
    trait_: Option<String>,
    functions: HashMap<String, TypeConstructor>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeType {
    Scalar,
    Trait,
    Struct,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnvType {
    generic: ast::Generic,
    is_a: TypeType,
    fields: HashMap<String, ast::TypeUsage>,
    impls: Vec<EnvImpl>,
}

impl EnvType {
    fn from_struct(struct_: &ast::StructTypeDeclaration) -> EnvType {
        return EnvType {
            generic: struct_.generic.clone(),
            is_a: TypeType::Struct,
            fields: struct_
                .fields
                .iter()
                .map(|field| (field.name.name.value.to_string(), field.type_.clone()))
                .collect(),
            impls: vec![],
        };
    }

    fn from_trait(trait_: &ast::TraitTypeDeclaration) -> EnvType {
        let mut functions = HashMap::new();
        for func in trait_.functions.iter() {
            match func {
                ast::TraitItem::FunctionDeclaration(fd) => {
                    functions.insert(fd.name.name.value.to_string(), TypeConstructor::from_declaration(&fd));
                }
                ast::TraitItem::Function(f) => {
                    functions.insert(f.declaration.name.name.value.to_string(), TypeConstructor::from_declaration(&f.declaration));
                }
            }
        }
        let impl_ = EnvImpl {
            trait_: Some(trait_.name.name.value.to_string()),
            functions: functions,
        };
        return EnvType {
            generic: trait_.generic.clone(),
            is_a: TypeType::Trait,
            fields: HashMap::new(),
            impls: vec![impl_],
        };
    }

    fn type_construct(&self, ctx: &Context, usage: &ast::GenericUsage) -> Result<EnvType> {
        // steps
        // 1. Check if matches bounds, create unknowns if necessary.
        // 2. Replace all (Named+generics or function args/return) recursively.
        // 3. Return updated, plus known generic usage to replace any unknown usage.
        let known_usage = match usage {
            ast::GenericUsage::Known(known) => known.clone(),
            ast::GenericUsage::Unknown => {
                let mut new_unknowns = vec!();
                for _ in 0..self.generic.parameters.len() {
                    new_unknowns.push(ast::TypeUsage::new_unknown(&ctx.id_generator));
                }
                ast::GenericInstantiation {
                    parameters: new_unknowns.iter().map(|tp| tp.clone()).collect(),
                }
            },
        };
        if known_usage.parameters.len() != self.generic.parameters.len() {
            return Err(errors::TypingError::WrongNumberOfTypeParameters{});
        }
        for i in 0..known_usage.parameters.len() {
            if !type_meets_trait_bounds(ctx, &known_usage.parameters[i], &self.generic.parameters[i].bounds) {
                return Err(errors::TypingError::InvalidTypeForGeneric);
            }
        }
        // Generic type matches, time to replace
        let mut result = self.clone();
        for i in 0..known_usage.parameters.len() {
            let mut fields = HashMap::new();
            for (k, v) in result.fields.iter() {
                let type_usage = replace_generic_with_concrete(&self.generic.parameters[i].name, &known_usage.parameters[i], &v);
                fields.insert(k.clone(), type_usage);
            }
            let mut impls = vec!();
            for impl_ in result.impls.iter() {
                let mut functions = HashMap::new();
                for (name, func) in impl_.functions.iter() {
                    let type_usage = replace_generic_with_concrete(&self.generic.parameters[i].name, &known_usage.parameters[i], &func.type_usage);

                    functions.insert(name.clone(), TypeConstructor{generic: func.generic.clone(), type_usage: type_usage});
                }
                impls.push(EnvImpl{
                    trait_: impl_.trait_.clone(),
                    functions: functions,
                });
            }
            result = EnvType{
                generic: ast::Generic{parameters: vec!()},
                is_a: self.is_a.clone(),
                fields: fields,
                impls: impls,
            };
        }
        return Ok(result);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NamedEntity {
    NamedType(EnvType),
    Function(TypeConstructor),
    Variable(ast::TypeUsage),
}

fn create_builtins() -> HashMap<String, NamedEntity> {
    let mut result = HashMap::new();
    result.insert(
        "i8".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "i16".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "i32".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "i64".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "isize".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );

    result.insert(
        "u8".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "u16".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "u32".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "u64".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "usize".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );

    result.insert(
        "f32".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "f64".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );

    result.insert(
        "bool".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "!".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "unit".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    result.insert(
        "String".to_string(),
        NamedEntity::NamedType(EnvType {
            generic: ast::Generic{parameters: vec!()},
            is_a: TypeType::Scalar,
            fields: HashMap::new(),
            impls: vec![],
        }),
    );
    return result;
}

enum StructAttr {
    Field(ast::TypeUsage),
    Method(TypeConstructor),
}

fn apply_self(type_name: &str, constructor: &TypeConstructor) -> TypeConstructor {
    match &constructor.type_usage {
        ast::TypeUsage::Function(func) => {
            if func.arguments.len() > 0 {
                match &func.arguments[0] {
                    ast::TypeUsage::Named(named) => {
                        if type_name == named.name.name.value {
                            let result = ast::TypeUsage::Function(ast::FunctionTypeUsage {
                                arguments: func.arguments[1..func.arguments.len()].iter().map(|arg| arg.clone()).collect(),
                                return_type: func.return_type.clone(),
                            });
                            return TypeConstructor{
                                generic: constructor.generic.clone(),
                                type_usage: result,
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        _ => {}
    }
    return constructor.clone();
}

fn get_attr(ctx: &Context, get_from: &NamedEntity, attribute: &ast::Identifier) -> Result<StructAttr> {
    match get_from {
        NamedEntity::NamedType(env_type) => {
            if env_type.fields.contains_key(&attribute.name.value) {
                return Ok(StructAttr::Field(env_type.fields[&attribute.name.value].clone()));
            }
            let mut result = Vec::new();
            for impl_ in env_type.impls.iter() {
                if impl_.functions.contains_key(&attribute.name.value) {
                    result.push(impl_.functions[&attribute.name.value].clone())
                }
            }
            if result.len() == 0 {
                return Err(errors::TypingError::UnknownFieldName {
                    identifier: attribute.clone(),
                });
            }
            if result.len() > 1 {
                return Err(errors::TypingError::MultipleFieldName {
                    identifier: attribute.clone(),
                });
            }
            return Ok(StructAttr::Method(result[0].clone()));
        }
        NamedEntity::Variable(type_) => match type_ {
            ast::TypeUsage::Named(named) => {
                let env_type = match &ctx.environment[&named.name.name.value] {
                    NamedEntity::NamedType(env_type) => env_type,
                    _ => panic!("variable has non-type as type"),
                };
                let type_ = env_type.type_construct(ctx, &named.type_parameters)?;

                let attr = get_attr(ctx, &NamedEntity::NamedType(type_), attribute)?;
                let method = match attr {
                    StructAttr::Field(field) => return Ok(StructAttr::Field(field)),
                    StructAttr::Method(method) => method,
                };
                return Ok(StructAttr::Method(apply_self(&named.name.name.value, &method)));
            }
            _ => {
                return Err(errors::TypingError::AttributeOfNonstruct {
                    identifier: attribute.clone(),
                });
            }
        },
        NamedEntity::Function(_) => {
            return Err(errors::TypingError::AttributeOfNonstruct {
                identifier: attribute.clone(),
            });
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    pub current_function_return: Option<ast::TypeUsage>,
    pub environment: HashMap<String, NamedEntity>,
    pub id_generator: Rc<ast::IdGenerator>,
}

impl Context {
    fn add_variable(&self, name: String, type_usage: &ast::TypeUsage) -> Context {
        let mut ctx = self.clone();
        ctx.environment.insert(name.to_string(), NamedEntity::Variable(type_usage.clone()));
        return ctx;
    }

    fn set_current_function_return(&self, function: &ast::TypeUsage) -> Context {
        let mut ctx = self.clone();
        ctx.current_function_return = Some(function.clone());
        return ctx;
    }

    fn add_generic(&self, generic: &ast::Generic) -> Result<Context> {
        let mut ctx = self.clone();
        for parameter in generic.parameters.iter() {
            let mut env_type = EnvType{
                generic: ast::Generic{
                    parameters: vec!(),
                },
                is_a: TypeType::Trait,
                fields: HashMap::new(),
                impls: vec!(),
            };
            for bound in parameter.bounds.iter() {
                if !self.environment.contains_key(&bound.name.value) {
                    return Err(errors::TypingError::TypeDoesNotExist {
                        identifier: bound.clone(),
                    });
                }
                match &self.environment[&bound.name.value] {
                    NamedEntity::NamedType(named_type) => {
                        env_type.impls.push(named_type.impls[0].clone());
                    },
                    _ => {}
                }
            };
            ctx.environment.insert(parameter.name.name.value.clone(), NamedEntity::NamedType(env_type));
        }
        return Ok(ctx);
    }

    fn add_impl(&self, impl_: &ast::Impl, traits: &HashMap<String, ast::TraitTypeDeclaration>) -> Result<Context> {
        let mut functions = HashMap::new();
        for func in impl_.functions.iter() {
            functions.insert(func.declaration.name.name.value.to_string(), TypeConstructor::from_declaration(&func.declaration));
        }
        // fill out defaults
        match &impl_.trait_ {
            Some(trait_) => {
                if !traits.contains_key(&trait_.name.name.value) {
                    return Err(errors::TypingError::TypeDoesNotExist {
                        identifier: trait_.name.clone(),
                    });
                }
                for func in traits[&trait_.name.name.value].functions.iter() {
                    match func {
                        ast::TraitItem::Function(default_function) => {
                            if !functions.contains_key(&default_function.declaration.name.name.value) {
                                functions.insert(
                                    default_function.declaration.name.name.value.to_string(),
                                    TypeConstructor::from_declaration(&default_function.declaration),
                                );
                            }
                        }
                        _ => {}
                    }
                }
            }
            None => {}
        }
        let mut result = self.clone();
        let mut env_named = result.environment[&impl_.struct_.name.name.value].clone();
        match &mut env_named {
            NamedEntity::NamedType(env_type) => {
                env_type.impls.push(EnvImpl {
                    trait_: match &impl_.trait_ {
                        Some(trait_) => Some(trait_.name.name.value.to_string()),
                        None => None,
                    },
                    functions: functions,
                });
                result
                    .environment
                    .insert(impl_.struct_.name.name.value.to_string(), NamedEntity::NamedType(env_type.clone()));
            }
            NamedEntity::Variable(_) => {
                return Err(errors::TypingError::IdentifierIsNotType {
                    identifier: impl_.struct_.name.clone(),
                });
            }
            NamedEntity::Function(_) => {
                return Err(errors::TypingError::IdentifierIsNotType {
                    identifier: impl_.struct_.name.clone(),
                });
            }
        }
        return Ok(result);
    }
}

fn type_exists(ctx: &Context, type_: &ast::TypeUsage) -> Result<()> {
    let result = match type_ {
        ast::TypeUsage::Named(named) => {
            if !ctx.environment.contains_key(&named.name.name.value) {
                return Err(errors::TypingError::TypeDoesNotExist {
                    identifier: named.name.clone(),
                });
            }
            match ctx.environment[&named.name.name.value] {
                NamedEntity::NamedType(_) => {
                    // is a type
                }
                _ => {
                    return Err(errors::TypingError::IdentifierIsNotType {
                        identifier: named.name.clone(),
                    });
                }
            }
        }
        ast::TypeUsage::Unknown(_) => {} // do nothing
        ast::TypeUsage::Namespace(_) => {}
        ast::TypeUsage::Function(function) => {
            let mut errs = vec![];
            for arg in function.arguments.iter() {
                match type_exists(ctx, arg) {
                    Ok(_) => {}
                    Err(err) => errs.push(err),
                };
            }
            match type_exists(ctx, &function.return_type) {
                Ok(_) => {}
                Err(err) => errs.push(err),
            }
            if errs.len() > 0 {
                return Err(errors::TypingError::MultipleErrors { errors: errs });
            }
        }
    };
    return Ok(result);
}

fn apply_substitution(ctx: &Context, substitution: &SubstitutionMap, type_: &ast::TypeUsage) -> Result<ast::TypeUsage> {
    let result = match type_ {
        ast::TypeUsage::Named(named) => ast::TypeUsage::Named(named.clone()),
        ast::TypeUsage::Unknown(unknown) => {
            if substitution.contains_key(&unknown.name) {
                substitution[&unknown.name].clone()
            } else {
                ast::TypeUsage::Unknown(unknown.clone())
            }
        }
        ast::TypeUsage::Namespace(namespace) => {
            ast::TypeUsage::Namespace(namespace.clone())
        }
        ast::TypeUsage::Function(function) => {
            let mut arguments = vec![];
            for arg in function.arguments.iter() {
                arguments.push(apply_substitution(ctx, substitution, arg)?);
            }
            ast::TypeUsage::Function(ast::FunctionTypeUsage {
                arguments: arguments,
                return_type: Box::new(apply_substitution(ctx, substitution, &function.return_type)?),
            })
        }
    };
    type_exists(ctx, &result)?;
    return Ok(result);
}

fn compose_substitutions(ctx: &Context, s1: &SubstitutionMap, s2: &SubstitutionMap) -> Result<SubstitutionMap> {
    let mut result = SubstitutionMap::new();
    for k in s2.keys() {
        result.insert(k.to_string(), apply_substitution(ctx, s1, &s2[k])?);
    }
    return Ok(s1.into_iter().map(|(k, v)| (k.clone(), v.clone())).chain(result).collect());
}

fn unify(ctx: &Context, t1: &ast::TypeUsage, t2: &ast::TypeUsage) -> Result<SubstitutionMap> {
    match (t1, t2) {
        (ast::TypeUsage::Named(named1), ast::TypeUsage::Named(named2)) => {
            if named1.name.name.value == named2.name.name.value {
                let mut result = SubstitutionMap::new();
                match (&named1.type_parameters, &named2.type_parameters) {
                    (ast::GenericUsage::Known(known1), ast::GenericUsage::Known(known2)) => {
                        if known1.parameters.len() != known2.parameters.len() {
                            return Err(errors::TypingError::TypeMismatch {
                                type_one: t1.clone(),
                                type_two: t2.clone(),
                            });
                        }
                        for (i, _) in known1.parameters.iter().enumerate() {
                            result = compose_substitutions(
                                ctx,
                                &result,
                                &unify(
                                    ctx,
                                    &apply_substitution(ctx, &result, &known1.parameters[i])?,
                                    &apply_substitution(ctx, &result, &known2.parameters[i])?,
                                )?,
                            )?;
                        }
                    },
                    _ => {
                        panic!("should never be unknown")
                    },
                }
                return Ok(result);
            }
        }
        _ => {}
    }
    match t1 {
        ast::TypeUsage::Unknown(unknown) => {
            return Ok(var_bind(&unknown.name, t2));
        }
        _ => {}
    }
    match t2 {
        ast::TypeUsage::Unknown(unknown) => {
            return Ok(var_bind(&unknown.name, t1));
        }
        _ => {}
    }
    match (t1, t2) {
        (ast::TypeUsage::Function(f1), ast::TypeUsage::Function(f2)) => {
            let mut result = unify(ctx, &*f1.return_type, &*f2.return_type)?;
            if f1.arguments.len() != f2.arguments.len() {
                panic!("Argument lengths don't match");
            }
            for (i, _) in f1.arguments.iter().enumerate() {
                result = compose_substitutions(
                    ctx,
                    &result,
                    &unify(
                        ctx,
                        &apply_substitution(ctx, &result, &f1.arguments[i])?,
                        &apply_substitution(ctx, &result, &f2.arguments[i])?,
                    )?,
                )?;
            }
            return Ok(result);
        }
        _ => {}
    }
    return Err(errors::TypingError::TypeMismatch {
        type_one: t1.clone(),
        type_two: t2.clone(),
    });
}

fn var_bind(name: &str, t: &ast::TypeUsage) -> SubstitutionMap {
    match t {
        ast::TypeUsage::Unknown(unknown) => {
            if name == unknown.name {
                return SubstitutionMap::new();
            }
        }
        _ => {}
    }
    if contains(t, name) {
        panic!("Type contains a reference to itself")
    }
    let mut substitution = SubstitutionMap::new();
    substitution.insert(name.to_string(), t.clone());
    return substitution;
}

fn contains(t: &ast::TypeUsage, name: &str) -> bool {
    match t {
        ast::TypeUsage::Named(_) => return false,
        ast::TypeUsage::Unknown(unknown) => unknown.name == name,
        ast::TypeUsage::Namespace(_) => return false,
        ast::TypeUsage::Function(f) => {
            if contains(&*f.return_type, name) {
                return true;
            }
            for arg in f.arguments.iter() {
                if contains(&arg.clone(), name) {
                    return true;
                }
            }
            return false;
        }
    }
}

pub struct TypeChecker {}

impl TypeChecker {
    pub fn with_module(self: &Self, module: &ast::Module) -> Result<(ast::Module, SubstitutionMap)> {
        let mut ctx = Context {
            environment: create_builtins(),
            current_function_return: None,
            id_generator: Rc::new(ast::IdGenerator::new("T")),
        };

        let mut traits = HashMap::new();

        for item in module.items.iter() {
            match item {
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Struct(struct_)) => {
                    ctx.environment.insert(
                        struct_.name.name.value.to_string(),
                        NamedEntity::NamedType(EnvType::from_struct(&struct_)),
                    );
                }
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Trait(trait_)) => {
                    traits.insert(trait_.name.name.value.to_string(), trait_.clone());
                    ctx.environment.insert(
                        trait_.name.name.value.to_string(),
                        NamedEntity::NamedType(EnvType::from_trait(&trait_)),
                    );
                }
                ast::ModuleItem::Function(function) => {
                    let function_type = ast::FunctionTypeUsage {
                        arguments: function.declaration.arguments.iter().map(|arg| arg.type_.clone()).collect(),
                        return_type: Box::new(function.declaration.return_type.clone()),
                    };
                    ctx.environment.insert(
                        function.declaration.name.name.value.to_string(),
                        NamedEntity::Function(TypeConstructor{
                            generic: function.declaration.generic.clone(),
                            type_usage: ast::TypeUsage::Function(function_type),
                        }),
                    );
                }
                _ => {}
            }
        }

        for item in module.items.iter() {
            match item {
                ast::ModuleItem::Impl(impl_) => {
                    if !ctx.environment.contains_key(&impl_.struct_.name.name.value) {
                        return Err(errors::TypingError::IdentifierIsNotType {
                            identifier: impl_.struct_.name.clone(),
                        });
                    }
                    ctx = ctx.add_impl(&impl_, &traits)?;
                }
                _ => {}
            }
        }

        let mut subst = SubstitutionMap::new();
        let mut items = vec![];
        for item in module.items.iter() {
            items.push(match item {
                ast::ModuleItem::Function(function) => {
                    let (func, fn_subst) = self.with_function(&ctx, &subst, function)?;
                    subst = compose_substitutions(&ctx, &subst, &fn_subst)?;
                    ast::ModuleItem::Function(func)
                }
                ast::ModuleItem::TypeDeclaration(type_declaration) => {
                    let (ty_decl, ty_subst) = self.with_type_declaration(&ctx, &subst, type_declaration)?;
                    subst = compose_substitutions(&ctx, &subst, &ty_subst)?;
                    ast::ModuleItem::TypeDeclaration(ty_decl)
                }
                ast::ModuleItem::Impl(impl_) => {
                    let (impl_result, impl_subst) = self.with_impl(&ctx, &subst, impl_)?;
                    // TODO: errors on generics not exist at global scope
                    // subst = compose_substitutions(&ctx, &subst, &impl_subst)?;
                    ast::ModuleItem::Impl(impl_result)
                }
            });
        }
        let result = ast::Module { items: items };
        return Ok((result, subst));
    }

    fn with_function_declaration(self: &Self, ctx: &Context, declaration: &ast::FunctionDeclaration) -> Result<ast::FunctionDeclaration> {
        for arg in declaration.arguments.iter() {
            type_exists(ctx, &arg.type_)?;
        }
        type_exists(ctx, &declaration.return_type)?;
        return Ok(declaration.clone());
    }

    fn with_function(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        function: &ast::Function,
    ) -> Result<(ast::Function, SubstitutionMap)> {
        let gen_ctx = ctx.add_generic(&function.declaration.generic)?;
        let declaration = self.with_function_declaration(&gen_ctx, &function.declaration)?;
        // add args to env
        let mut function_ctx = gen_ctx.set_current_function_return(&declaration.return_type.clone());
        for arg in declaration.arguments.iter() {
            function_ctx = function_ctx.add_variable(arg.name.name.value.to_string(), &arg.type_.clone());
        }

        let (block, substitution) = self.with_block(&function_ctx, incoming_substitutions, &function.block)?;
        let mut substitution = compose_substitutions(&function_ctx, incoming_substitutions, &substitution)?;
        match &block.type_ {
            ast::TypeUsage::Named(named) => {
                if named.name.name.value != "!" {
                    substitution = compose_substitutions(
                        &function_ctx,
                        &substitution,
                        &unify(&function_ctx, &declaration.return_type, &block.type_)?,
                    )?;
                }
            }
            _ => {
                substitution = compose_substitutions(
                    &function_ctx,
                    &substitution,
                    &unify(&function_ctx, &declaration.return_type, &block.type_)?,
                )?;
            }
        }

        return Ok((
            ast::Function {
                declaration: ast::FunctionDeclaration {
                    generic: declaration.generic.clone(),
                    name: declaration.name.clone(),
                    arguments: declaration.arguments.iter().map(|arg| arg.clone()).collect(),
                    return_type: declaration.return_type.clone(),
                },
                block: block,
            },
            substitution,
        ));
    }

    fn with_type_declaration(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        type_declaration: &ast::TypeDeclaration,
    ) -> Result<(ast::TypeDeclaration, SubstitutionMap)> {
        match type_declaration {
            ast::TypeDeclaration::Struct(struct_) => {
                let result = self.with_struct_declaration(ctx, struct_)?;
                return Ok((ast::TypeDeclaration::Struct(result), SubstitutionMap::new()));
            }
            ast::TypeDeclaration::Primitive(primitive) => {
                return Ok((ast::TypeDeclaration::Primitive(primitive.clone()), SubstitutionMap::new()));
            }
            ast::TypeDeclaration::Alias(alias) => {
                return Ok((ast::TypeDeclaration::Alias(alias.clone()), SubstitutionMap::new()));
            }
            ast::TypeDeclaration::Trait(trait_) => {
                let (result, subst) = self.with_trait_declaration(ctx, incoming_substitutions, trait_)?;
                return Ok((ast::TypeDeclaration::Trait(result), subst));
            }
        }
    }

    fn with_trait_declaration(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        trait_: &ast::TraitTypeDeclaration,
    ) -> Result<(ast::TraitTypeDeclaration, SubstitutionMap)> {
        let gen_ctx = ctx.add_generic(&trait_.generic)?;
        let mut substitutions = incoming_substitutions.clone();
        let mut result_functions = vec![];
        for item in &trait_.functions {
            match item {
                ast::TraitItem::FunctionDeclaration(declaration) => {
                    let result_declaration = self.with_function_declaration(&gen_ctx, declaration)?;
                    result_functions.push(ast::TraitItem::FunctionDeclaration(result_declaration));
                }
                ast::TraitItem::Function(function) => {
                    let (function_result, susbt) = self.with_function(&gen_ctx, incoming_substitutions, function)?;
                    substitutions = compose_substitutions(ctx, &substitutions, &susbt)?;
                    result_functions.push(ast::TraitItem::Function(function_result));
                }
            }
        }
        Ok((
            ast::TraitTypeDeclaration {
                generic: trait_.generic.clone(),
                name: trait_.name.clone(),
                functions: result_functions,
            },
            substitutions,
        ))
    }

    fn with_struct_declaration(self: &Self, ctx: &Context, struct_: &ast::StructTypeDeclaration) -> Result<ast::StructTypeDeclaration> {
        let struct_ctx = ctx.add_generic(&struct_.generic)?;
        let mut fields = vec![];
        for field in struct_.fields.iter() {
            type_exists(&struct_ctx, &field.type_)?;
            fields.push(ast::StructField {
                name: field.name.clone(),
                type_: field.type_.clone(),
            });
        }
        return Ok(ast::StructTypeDeclaration {
            generic: struct_.generic.clone(),
            name: struct_.name.clone(),
            fields: fields,
        });
    }

    fn with_impl(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        impl_: &ast::Impl,
    ) -> Result<(ast::Impl, SubstitutionMap)> {
        let impl_ctx = ctx.add_generic(&impl_.generic)?;

        let mut substitutions = incoming_substitutions.clone();
        type_exists(
            &impl_ctx,
            &ast::TypeUsage::new_named(&impl_.struct_.name.clone(), &ast::GenericUsage::Unknown),
        )?;
        let mut functions = vec![];
        for function in impl_.functions.iter() {
            let (result, function_subs) = self.with_function(&impl_ctx, &substitutions, function)?;
            substitutions = compose_substitutions(&impl_ctx, &substitutions, &function_subs)?;
            functions.push(result);
        }
        return Ok((
            ast::Impl {
                generic: impl_.generic.clone(),
                trait_: impl_.trait_.clone(),
                struct_: impl_.struct_.clone(),
                functions: functions,
            },
            substitutions,
        ));
    }

    fn with_block(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        block: &ast::Block,
    ) -> Result<(ast::Block, SubstitutionMap)> {
        let mut substitutions = incoming_substitutions.clone();
        let mut block_ctx = ctx.clone();
        // if return it's always never
        // if last is expression it's that else unit
        let mut has_return = false;
        for statement in block.statements.iter() {
            match statement {
                ast::Statement::Return(_) => {
                    has_return = true;
                }
                _ => {}
            }
        }
        let mut statements = vec![];
        for s in block.statements.iter() {
            let (statement_ctx, result, statement_substitutions) = self.with_statement(&block_ctx, &substitutions, s)?;
            block_ctx = statement_ctx;
            substitutions = compose_substitutions(&block_ctx, &substitutions, &statement_substitutions)?;
            statements.push(result);
        }
        if !has_return {
            match block.statements.last().unwrap() {
                ast::Statement::Expression(expr) => {
                    substitutions = compose_substitutions(&block_ctx, &substitutions, &unify(&block_ctx, &block.type_, &expr.type_)?)?;
                }
                _ => {
                    substitutions = compose_substitutions(&block_ctx, &substitutions, &unify(&block_ctx, &block.type_, &ast::new_unit())?)?;
                }
            }
        }
        let result_type = if has_return {
            ast::new_never()
        } else {
            apply_substitution(&block_ctx, &substitutions, &block.type_)?
        };
        let block_result = ast::Block {
            statements: statements,
            type_: result_type,
        };
        return Ok((block_result, substitutions));
    }

    fn with_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::Statement,
    ) -> Result<(Context, ast::Statement, SubstitutionMap)> {
        match statement {
            ast::Statement::Return(return_statement) => {
                let (result, subst) = self.with_return_statement(ctx, incoming_substitutions, return_statement)?;
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst)?;
                return Ok((ctx.clone(), ast::Statement::Return(result), subst));
            }
            ast::Statement::Let(let_statement) => {
                let (let_ctx, result, subst) = self.with_let_statement(ctx, incoming_substitutions, let_statement)?;
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst)?;
                return Ok((let_ctx, ast::Statement::Let(result), subst));
            }
            ast::Statement::Assignment(assignment_statement) => {
                let (result, subst) = self.with_assignment_statement(ctx, incoming_substitutions, assignment_statement)?;
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst)?;
                return Ok((ctx.clone(), ast::Statement::Assignment(result), subst));
            }
            ast::Statement::Expression(expression) => {
                let (result, subst) = self.with_expression(ctx, incoming_substitutions, expression)?;
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst)?;
                return Ok((ctx.clone(), ast::Statement::Expression(result), subst));
            }
        }
    }

    fn with_return_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::ReturnStatement,
    ) -> Result<(ast::ReturnStatement, SubstitutionMap)> {
        let (result, subst) = self.with_expression(ctx, incoming_substitutions, &statement.source)?;
        let mut substitution = compose_substitutions(ctx, &incoming_substitutions, &subst)?;
        let mut is_never = false;
        match &result.type_ {
            ast::TypeUsage::Named(named) => {
                if named.name.name.value == "!" {
                    is_never = true;
                }
            }
            _ => {}
        }
        if !is_never {
            substitution = compose_substitutions(
                ctx,
                &subst,
                &unify(ctx, &ctx.current_function_return.as_ref().unwrap(), &result.type_)?,
            )?;
        }

        return Ok((ast::ReturnStatement { source: result }, substitution));
    }

    fn with_let_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::LetStatement,
    ) -> Result<(Context, ast::LetStatement, SubstitutionMap)> {
        let (result, subst) = self.with_expression(ctx, incoming_substitutions, &statement.expression)?;
        let let_ctx = ctx.add_variable(statement.variable_name.name.value.clone(), &result.type_);
        let substitution = compose_substitutions(ctx, &subst, &unify(ctx, &statement.type_, &result.type_)?)?;
        return Ok((
            let_ctx,
            ast::LetStatement {
                variable_name: statement.variable_name.clone(),
                expression: result,
                type_: apply_substitution(ctx, &substitution, &statement.type_)?,
            },
            substitution,
        ));
    }

    fn with_assignment_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::AssignmentStatement,
    ) -> Result<(ast::AssignmentStatement, SubstitutionMap)> {
        let (expr, subst) = self.with_expression(ctx, incoming_substitutions, &statement.expression)?;
        let mut substitution = compose_substitutions(ctx, &incoming_substitutions, &subst)?;

        let result_as = ast::AssignmentStatement {
            source: match &statement.source {
                ast::AssignmentTarget::Variable(variable) => {
                    substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &variable.type_, &expr.type_)?)?;
                    ast::AssignmentTarget::Variable(ast::VariableUsage {
                        name: variable.name.clone(),
                        type_parameters: variable.type_parameters.clone(),
                        type_: apply_substitution(ctx, &substitution, &variable.type_)?,
                    })
                }
                ast::AssignmentTarget::StructAttr(struct_attr) => {
                    let (source, subst) = self.with_expression(ctx, &substitution, &struct_attr.source)?;
                    let mut subst = subst.clone();

                    let field_type = match get_attr(ctx, &NamedEntity::Variable(source.type_.clone()), &struct_attr.attribute)? {
                        StructAttr::Field(type_) => type_,
                        StructAttr::Method(_) => {
                            return Err(errors::TypingError::CannotAssignToMethod {
                                identifier: struct_attr.attribute.clone(),
                            })
                        }
                    };

                    subst = compose_substitutions(ctx, &subst, &unify(ctx, &struct_attr.type_, &field_type)?)?;

                    let substitution = compose_substitutions(
                        ctx,
                        &compose_substitutions(ctx, &substitution, &subst)?,
                        &unify(ctx, &struct_attr.type_, &expr.type_)?,
                    )?;
                    ast::AssignmentTarget::StructAttr(ast::StructGetter {
                        type_parameters: struct_attr.type_parameters.clone(),
                        source: source,
                        attribute: struct_attr.attribute.clone(),
                        type_: apply_substitution(ctx, &substitution, &struct_attr.type_)?,
                    })
                }
            },
            expression: expr,
        };
        return Ok((result_as, substitution));
    }

    fn with_expression(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        expression: &ast::Expression,
    ) -> Result<(ast::Expression, SubstitutionMap)> {
        let mut substitution = incoming_substitutions.clone();
        let subexpression = Box::new(match &*expression.subexpression {
            ast::Subexpression::LiteralInt(literal_int) => {
                let (result, subst) = self.with_literal_int(ctx, &substitution, literal_int)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::LiteralInt(result)
            }
            ast::Subexpression::LiteralFloat(literal_float) => {
                let (result, subst) = self.with_literal_float(ctx, &substitution, literal_float)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::LiteralFloat(result)
            }
            ast::Subexpression::LiteralBool(literal_bool) => {
                let (result, subst) = self.with_literal_bool(ctx, &substitution, literal_bool)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::LiteralBool(result)
            }
            ast::Subexpression::LiteralString(literal_string) => {
                let (result, subst) = self.with_literal_string(ctx, &substitution, literal_string)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::LiteralString(result)
            }
            ast::Subexpression::LiteralStruct(literal_struct) => {
                let (result, subst) = self.with_literal_struct(ctx, &substitution, literal_struct)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::LiteralStruct(result)
            }
            ast::Subexpression::FunctionCall(function_call) => {
                let (result, subst) = self.with_function_call(ctx, &substitution, function_call)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &function_call.type_)?)?;
                ast::Subexpression::FunctionCall(result)
            }
            ast::Subexpression::VariableUsage(variable_usage) => {
                let (result, subst) = self.with_variable_usage(ctx, &substitution, variable_usage)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &variable_usage.type_)?)?;
                ast::Subexpression::VariableUsage(result)
            }
            ast::Subexpression::If(if_expression) => {
                let (result, subst) = self.with_if(ctx, &substitution, if_expression)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::If(result)
            }
            ast::Subexpression::StructGetter(struct_getter) => {
                let (result, subst) = self.with_struct_getter(ctx, &substitution, struct_getter)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::StructGetter(result)
            }
            ast::Subexpression::Block(block) => {
                let (result, subst) = self.with_block_expression(ctx, &substitution, block)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.type_)?)?;
                ast::Subexpression::Block(result)
            }
            ast::Subexpression::Op(op) => {
                let (result, subst) = self.with_op(ctx, &substitution, op)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.left.type_)?)?;
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &expression.type_, &result.right.type_)?)?;
                ast::Subexpression::Op(result)
            }
        });

        let expr = ast::Expression {
            subexpression: subexpression,
            type_: apply_substitution(ctx, &substitution, &expression.type_)?,
        };
        return Ok((expr, substitution));
    }

    fn with_literal_int(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        literal_int: &ast::LiteralInt,
    ) -> Result<(ast::LiteralInt, SubstitutionMap)> {
        Ok((
            ast::LiteralInt {
                value: literal_int.value.clone(),
                type_: apply_substitution(ctx, &substitution, &literal_int.type_)?,
            },
            substitution.clone(),
        ))
    }

    fn with_literal_float(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        literal_float: &ast::LiteralFloat,
    ) -> Result<(ast::LiteralFloat, SubstitutionMap)> {
        Ok((
            ast::LiteralFloat {
                value: literal_float.value.clone(),
                type_: apply_substitution(ctx, &substitution, &literal_float.type_)?,
            },
            substitution.clone(),
        ))
    }

    fn with_literal_bool(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        literal_bool: &ast::LiteralBool,
    ) -> Result<(ast::LiteralBool, SubstitutionMap)> {
        Ok((
            ast::LiteralBool {
                value: literal_bool.value.clone(),
                type_: apply_substitution(ctx, &substitution, &literal_bool.type_)?,
            },
            substitution.clone(),
        ))
    }

    fn with_literal_string(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        literal_string: &ast::LiteralString,
    ) -> Result<(ast::LiteralString, SubstitutionMap)> {
        Ok((
            ast::LiteralString {
                value: literal_string.value.clone(),
                type_: apply_substitution(ctx, &substitution, &literal_string.type_)?,
            },
            substitution.clone(),
        ))
    }

    fn with_literal_struct(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        literal_struct: &ast::LiteralStruct,
    ) -> Result<(ast::LiteralStruct, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let struct_type = match &ctx.environment[&literal_struct.name.name.value] {
            NamedEntity::NamedType(env_type) => match &env_type.is_a {
                TypeType::Struct => env_type.type_construct(ctx, &literal_struct.type_parameters)?,
                _ => {
                    return Err(errors::TypingError::NotAStructLiteral {
                        identifier: literal_struct.name.clone(),
                    });
                }
            },
            _ => {
                return Err(errors::TypingError::NotAStructLiteral {
                    identifier: literal_struct.name.clone(),
                });
            }
        };

        if struct_type.fields.len() != literal_struct.fields.len() {
            return Err(errors::TypingError::StructLiteralFieldsMismatch {
                struct_name: literal_struct.name.clone(),
            });
        }
        let mut fields = vec![];
        for (type_field_name, type_field_type) in struct_type.fields.iter() {
            let mut found = false;
            let mut field_expression: Option<ast::Expression> = None;
            let mut field_name: Option<ast::Identifier> = None;
            for field in literal_struct.fields.iter() {
                if type_field_name == &field.0.name.value {
                    found = true;
                    let (result, subst) = self.with_expression(ctx, &substitution, &field.1)?;
                    substitution = compose_substitutions(ctx, &substitution, &subst)?;
                    substitution = compose_substitutions(ctx, &substitution, &unify(ctx, type_field_type, &result.type_)?)?;
                    field_expression = Some(result);
                    field_name = Some(field.0.clone());
                }
            }
            if !found {
                return Err(errors::TypingError::StructLiteralFieldsMismatch {
                    struct_name: literal_struct.name.clone(),
                });
            }
            fields.push((field_name.unwrap(), field_expression.unwrap()));
        }
        Ok((
            ast::LiteralStruct {
                type_parameters: literal_struct.type_parameters.clone(),
                name: literal_struct.name.clone(),
                fields: fields,
                type_: apply_substitution(ctx, &substitution, &literal_struct.type_)?,
            },
            substitution,
        ))
    }

    fn with_function_call(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        function_call: &ast::FunctionCall,
    ) -> Result<(ast::FunctionCall, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let (source, subst) = self.with_expression(ctx, &substitution, &function_call.source)?;
        substitution = compose_substitutions(ctx, &substitution, &subst)?;
        match &source.type_ {
            ast::TypeUsage::Function(fn_type) => {
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &function_call.type_, &*fn_type.return_type)?)?;
                if function_call.arguments.len() != fn_type.arguments.len() {
                    return Err(errors::TypingError::ArgumentLengthMismatch {});
                }
            }
            ast::TypeUsage::Named(_) => {
                return Err(errors::TypingError::FunctionCallNotAFunction {});
            }
            _ => {}
        }
        let mut arguments = vec![];
        for (i, arg) in function_call.arguments.iter().enumerate() {
            let (result, subst) = self.with_expression(ctx, &substitution, arg)?;
            substitution = compose_substitutions(ctx, &substitution, &subst)?;

            match &source.type_ {
                ast::TypeUsage::Function(fn_type) => {
                    substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &fn_type.arguments[i], &result.type_)?)?;
                }
                ast::TypeUsage::Named(_) => {
                    return Err(errors::TypingError::FunctionCallNotAFunction {});
                }
                _ => {}
            }
            arguments.push(result);
        }
        Ok((
            ast::FunctionCall {
                source: source.clone(),
                arguments: arguments,
                type_: apply_substitution(ctx, &substitution, &function_call.type_)?,
            },
            substitution,
        ))
    }

    fn with_variable_usage(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        variable_usage: &ast::VariableUsage,
    ) -> Result<(ast::VariableUsage, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        match &ctx.environment[&variable_usage.name.name.value] {
            NamedEntity::NamedType(named_type) => {
                let type_ = ast::TypeUsage::Namespace(ast::NamespaceTypeUsage::Type(ast::NamedTypeUsage{name: variable_usage.name.clone(), type_parameters: variable_usage.type_parameters.clone()}));
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &variable_usage.type_, &type_)?)?;
            }
            NamedEntity::Variable(variable) => {
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &variable_usage.type_, &variable)?)?;
            }
            NamedEntity::Function(function) => {
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &variable_usage.type_, &function.construct(ctx, &variable_usage.type_parameters)?)?)?;
            }
        }
        Ok((
            ast::VariableUsage {
                name: variable_usage.name.clone(),
                type_parameters: variable_usage.type_parameters.clone(), // Redundant to type
                type_: apply_substitution(ctx, &substitution, &variable_usage.type_)?,
            },
            substitution,
        ))
    }

    fn with_if(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        if_expression: &ast::IfExpression,
    ) -> Result<(ast::IfExpression, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let (condition, subst) = self.with_expression(ctx, &substitution, &if_expression.condition)?;
        substitution = compose_substitutions(ctx, &substitution, &subst)?;

        let (block_result, subst) = self.with_block(ctx, &substitution, &if_expression.block)?;
        substitution = compose_substitutions(ctx, &substitution, &subst)?;

        let else_ = match &if_expression.else_ {
            Some(else_) => {
                let (result, subst) = self.with_block(ctx, &substitution, else_)?;
                substitution = compose_substitutions(ctx, &substitution, &subst)?;
                Some(result)
            }
            None => None,
        };

        match &condition.type_ {
            ast::TypeUsage::Named(named) => {
                if named.name.name.value != "bool" {
                    return Err(errors::TypingError::IfConditionMustBeBool {});
                }
            }
            ast::TypeUsage::Function(_) => {
                return Err(errors::TypingError::IfConditionMustBeBool {});
            }
            _ => {}
        };

        let mut never_count = 0;
        match &block_result.type_ {
            ast::TypeUsage::Named(named) => {
                if named.name.name.value != "!" {
                    substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &if_expression.type_, &block_result.type_)?)?;
                } else {
                    never_count += 1;
                }
            }
            _ => {
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &if_expression.type_, &block_result.type_)?)?;
            }
        };

        match &else_ {
            Some(else_block) => {
                match &else_block.type_ {
                    ast::TypeUsage::Named(named) => {
                        if named.name.name.value != "!" {
                            substitution =
                                compose_substitutions(ctx, &substitution, &unify(ctx, &if_expression.type_, &else_block.type_)?)?;
                        } else {
                            never_count += 1;
                        }
                    }
                    _ => {
                        substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &if_expression.type_, &else_block.type_)?)?;
                    }
                };
            }
            None => {
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &if_expression.type_, &ast::new_unit())?)?;
            }
        }

        let result_type = if never_count == 2 {
            ast::new_never()
        } else {
            apply_substitution(ctx, &substitution, &if_expression.type_)?
        };

        Ok((
            ast::IfExpression {
                condition: condition,
                block: block_result,
                else_: else_,
                type_: result_type,
            },
            substitution,
        ))
    }

    fn with_struct_getter(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        struct_getter: &ast::StructGetter,
    ) -> Result<(ast::StructGetter, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let (source, subst) = self.with_expression(ctx, &substitution, &struct_getter.source)?;
        substitution = compose_substitutions(ctx, &substitution, &subst)?;

        let field_type = match get_attr(ctx, &NamedEntity::Variable(source.type_.clone()), &struct_getter.attribute)? {
            StructAttr::Field(type_) => type_,
            StructAttr::Method(constructor) => constructor.construct(ctx, &struct_getter.type_parameters)?,
        };

        substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &struct_getter.type_, &field_type)?)?;

        Ok((
            ast::StructGetter {
                type_parameters: struct_getter.type_parameters.clone(),
                source: source,
                attribute: struct_getter.attribute.clone(),
                type_: apply_substitution(ctx, &substitution, &struct_getter.type_)?,
            },
            substitution,
        ))
    }

    fn with_block_expression(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        block: &ast::Block,
    ) -> Result<(ast::Block, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let (result, subst) = self.with_block(ctx, &substitution, &block)?;
        substitution = compose_substitutions(ctx, &substitution, &subst)?;
        Ok((result, substitution))
    }

    fn with_op(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        op: &ast::Operation,
    ) -> Result<(ast::Operation, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let (expr_left, subst_left) = self.with_expression(ctx, &substitution, &op.left)?;
        let (expr_right, subst_right) = self.with_expression(ctx, &substitution, &op.right)?;
        substitution = compose_substitutions(ctx, &substitution, &subst_left)?;
        substitution = compose_substitutions(ctx, &substitution, &subst_right)?;
        Ok((
            ast::Operation {
                left: expr_left,
                op: op.op.clone(),
                right: expr_right,
            },
            substitution,
        ))
    }
}
