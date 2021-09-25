use crate::ast;
use crate::errors;
use std::collections::HashMap;

pub type SubstitutionMap = HashMap<String, ast::TypeUsage>;

pub type Result<T, E = errors::TypingError> = std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamedEntity {
    TypeDeclaration(ast::TypeDeclaration),
    Variable(ast::TypeUsage),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    pub current_function_return: Option<ast::TypeUsage>,
    pub impls: Vec<ast::Impl>,
    pub environment: HashMap<String, NamedEntity>,
}

fn create_builtins() -> HashMap<String, NamedEntity> {
    let mut result = HashMap::new();
    result.insert(
        "i8".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "i8".to_string(),
        })),
    );
    result.insert(
        "i16".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "i16".to_string(),
        })),
    );
    result.insert(
        "i32".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "i32".to_string(),
        })),
    );
    result.insert(
        "i64".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "i64".to_string(),
        })),
    );
    result.insert(
        "isize".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "isize".to_string(),
        })),
    );

    result.insert(
        "u8".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "u8".to_string(),
        })),
    );
    result.insert(
        "u16".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "u16".to_string(),
        })),
    );
    result.insert(
        "u32".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "u32".to_string(),
        })),
    );
    result.insert(
        "u64".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "u64".to_string(),
        })),
    );
    result.insert(
        "usize".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "usize".to_string(),
        })),
    );

    result.insert(
        "f32".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "f32".to_string(),
        })),
    );
    result.insert(
        "f64".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "f64".to_string(),
        })),
    );

    result.insert(
        "bool".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "bool".to_string(),
        })),
    );

    result.insert(
        "!".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "!".to_string(),
        })),
    );
    result.insert(
        "unit".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "unit".to_string(),
        })),
    );

    return result;
}

enum StructAttr {
    Field(ast::TypeUsage),
    Method(ast::TypeUsage),
}

fn get_struct_attr(ctx: &Context, struct_declaration: &ast::StructTypeDeclaration, attribute: &ast::Identifier) -> Result<StructAttr> {
    for field in struct_declaration.fields.iter() {
        if field.name.name.value == attribute.name.value {
            return Ok(StructAttr::Field(field.type_.clone()));
        }
    }

    let mut result = Vec::new();
    for impl_ in ctx.impls.iter() {
        if &struct_declaration.name.name.value != &impl_.struct_name.name.value {
            continue;
        }
        for method in impl_.functions.iter() {
            if method.declaration.name.name.value == attribute.name.value {
                let mut function_type = method.declaration.to_type();

                // if the name of the type of the first argument == the class, remove the first arg
                if method.declaration.arguments.len() > 0 {
                    match &method.declaration.arguments[0].type_ {
                        ast::TypeUsage::Named(named) => {
                            if named.name.name.value == struct_declaration.name.name.value {
                                function_type = method.declaration.to_method_type();
                            }
                        }
                        _ => {}
                    };
                }
                result.push(function_type);
            }
        }
    }
    // TODO: default trait impls
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

fn get_trait_attr(ctx: &Context, trait_declaration: &ast::TraitTypeDeclaration, attribute: &ast::Identifier) -> Result<StructAttr> {
    let mut result = Vec::new();
    for trait_item in trait_declaration.functions.iter() {
        let declaration = match trait_item {
            ast::TraitItem::Function(function) => &function.declaration,
            ast::TraitItem::FunctionDeclaration(declaration) => declaration,
        };
        if declaration.name.name.value == attribute.name.value {
            let mut function_type = declaration.to_type();
            println!("foo: {:?}", declaration);
            // if the name of the type of the first argument == the class, remove the first arg
            if declaration.arguments.len() > 0 {
                match &declaration.arguments[0].type_ {
                    ast::TypeUsage::Named(named) => {
                        if named.name.name.value == trait_declaration.name.name.value {
                            function_type = declaration.to_method_type();
                        }
                    }
                    _ => {}
                };
            }
            result.push(function_type);
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

fn get_attr(ctx: &Context, source_type: &ast::TypeUsage, attribute: &ast::Identifier) -> Result<StructAttr> {
    match source_type {
        ast::TypeUsage::Named(named) => {
            match &ctx.environment[&named.name.name.value] {
                NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(type_declaration)) => {
                    return get_struct_attr(ctx, type_declaration, attribute);
                }
                NamedEntity::TypeDeclaration(ast::TypeDeclaration::Trait(type_declaration)) => {
                    return get_trait_attr(ctx, type_declaration, attribute);
                }
                _ => {
                    return Err(errors::TypingError::AttributeOfNonstruct {
                        identifier: attribute.clone(),
                    });
                    // TODO: support builtins - float, int, etc.
                }
            }
        }
        ast::TypeUsage::Function(_) => {
            return Err(errors::TypingError::NotAStructLiteral {
                identifier: attribute.clone(),
            });
        }
        _ => {
            panic!("tried to get attr of unknown");
        }
    };
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
                NamedEntity::TypeDeclaration(_) => {
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
                return Ok(SubstitutionMap::new());
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
            impls: Vec::new(),
            current_function_return: None,
        };

        for item in module.items.iter() {
            match item {
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Struct(struct_)) => {
                    ctx.environment.insert(
                        struct_.name.name.value.to_string(),
                        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(struct_.clone())),
                    );
                }
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Alias(alias)) => {
                    ctx.environment.insert(
                        alias.name.name.value.to_string(),
                        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Alias(alias.clone())),
                    );
                }
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Trait(trait_)) => {
                    ctx.environment.insert(
                        trait_.name.name.value.to_string(),
                        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Trait(trait_.clone())),
                    );
                }
                ast::ModuleItem::Function(function) => {
                    let function_type = ast::FunctionTypeUsage {
                        arguments: function.declaration.arguments.iter().map(|arg| arg.type_.clone()).collect(),
                        return_type: Box::new(function.declaration.return_type.clone()),
                    };
                    ctx.environment.insert(
                        function.declaration.name.name.value.to_string(),
                        NamedEntity::Variable(ast::TypeUsage::Function(function_type)),
                    );
                }
                ast::ModuleItem::Impl(impl_) => {
                    ctx.impls.push(impl_.clone());
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
                    subst = compose_substitutions(&ctx, &subst, &impl_subst)?;
                    ast::ModuleItem::Impl(impl_result)
                }
            });
        }
        let result = ast::Module { items: items };
        return Ok((result, subst));
    }

    fn with_function_declaration(
        self: &Self,
        ctx: &Context,
        declaration: &ast::FunctionDeclaration,
    ) -> Result<ast::FunctionDeclaration> {
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
        let declaration = self.with_function_declaration(ctx, &function.declaration)?;
        // add args to env
        let mut function_ctx = ctx.set_current_function_return(&declaration.return_type.clone());
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
        let mut substitutions = incoming_substitutions.clone();
        let mut result_functions = vec!();
        for item in &trait_.functions {
            match item {
                ast::TraitItem::FunctionDeclaration(declaration) => {
                    let result_declaration = self.with_function_declaration(ctx, declaration)?;
                    result_functions.push(ast::TraitItem::FunctionDeclaration(result_declaration));
                }
                ast::TraitItem::Function(function) => {
                    let (function_result, susbt) = self.with_function(ctx, incoming_substitutions, function)?;
                    substitutions = compose_substitutions(ctx, &substitutions, &susbt)?;
                    result_functions.push(ast::TraitItem::Function(function_result));
                }
            }
        }
        Ok((
            ast::TraitTypeDeclaration {
                name: trait_.name.clone(),
                functions: result_functions,
            },
            substitutions,
        ))
    }

    fn with_struct_declaration(self: &Self, ctx: &Context, struct_: &ast::StructTypeDeclaration) -> Result<ast::StructTypeDeclaration> {
        let mut fields = vec![];
        for field in struct_.fields.iter() {
            type_exists(ctx, &field.type_)?;
            fields.push(ast::StructField {
                name: field.name.clone(),
                type_: field.type_.clone(),
            });
        }
        return Ok(ast::StructTypeDeclaration {
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
        let mut substitutions = incoming_substitutions.clone();
        type_exists(ctx, &ast::TypeUsage::new_named(impl_.struct_name.clone()))?;
        let mut functions = vec![];
        for function in impl_.functions.iter() {
            let (result, function_subs) = self.with_function(&ctx, &substitutions, function)?;
            substitutions = compose_substitutions(ctx, &substitutions, &function_subs)?;
            functions.push(result);
        }
        // See if trait actually matches
        match &impl_.trait_ {
            Some(trait_) => {
                // assert trait functions satisfied
                if !ctx.environment.contains_key(&trait_.name.value) {
                    return Err(errors::TypingError::TypeDoesNotExist {
                        identifier: trait_.clone(),
                    });
                }
                let trait_declaration = match &ctx.environment[&trait_.name.value] {
                    NamedEntity::TypeDeclaration(ast::TypeDeclaration::Trait(declaration)) => declaration,
                    _ => {
                        return Err(errors::TypingError::ImplTraitMustBeTrait {
                            trait_name: trait_.clone(),
                        });
                    }
                };
                for trait_item in trait_declaration.functions.iter() {
                    match trait_item {
                        ast::TraitItem::FunctionDeclaration(declaration) => {
                            let mut found = false;
                            for impl_function in impl_.functions.iter() {
                                if impl_function.declaration.name.name.value == declaration.name.name.value {
                                    found = true;
                                    compare_struct_trait(&impl_function.declaration.to_type(), &declaration.to_type(), &impl_.struct_name, &trait_)?;
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
                                    compare_struct_trait(&impl_function.declaration.to_type(), &function.declaration.to_type(), &impl_.struct_name, &trait_)?;
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
                            ast::TraitItem::FunctionDeclaration(declaration) => declaration,
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
        return Ok((
            ast::Impl {
                trait_: impl_.trait_.clone(),
                struct_name: impl_.struct_name.clone(),
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
                        type_: apply_substitution(ctx, &substitution, &variable.type_)?,
                    })
                }
                ast::AssignmentTarget::StructAttr(struct_attr) => {
                    let (source, subst) = self.with_expression(ctx, &substitution, &struct_attr.source)?;
                    let mut subst = subst.clone();

                    let field_type = match get_attr(ctx, &source.type_, &struct_attr.attribute)? {
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

    fn with_literal_struct(
        self: &Self,
        ctx: &Context,
        substitution: &SubstitutionMap,
        literal_struct: &ast::LiteralStruct,
    ) -> Result<(ast::LiteralStruct, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let type_declaration = match &ctx.environment[&literal_struct.name.name.value] {
            NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(type_declaration)) => type_declaration,
            _ => {
                return Err(errors::TypingError::NotAStructLiteral {
                    identifier: literal_struct.name.clone(),
                });
            }
        };
        if type_declaration.fields.len() != literal_struct.fields.len() {
            return Err(errors::TypingError::StructLiteralFieldsMismatch {
                struct_name: literal_struct.name.clone(),
                struct_definition_name: type_declaration.name.clone(),
            });
        }
        let mut fields = vec![];
        for type_field in type_declaration.fields.iter() {
            let mut found = false;
            let mut field_expression: Option<ast::Expression> = None;
            for field in literal_struct.fields.iter() {
                if type_field.name.name.value == field.0.name.value {
                    found = true;
                    let (result, subst) = self.with_expression(ctx, &substitution, &field.1)?;
                    substitution = compose_substitutions(ctx, &substitution, &subst)?;
                    substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &type_field.type_, &result.type_)?)?;
                    field_expression = Some(result);
                }
            }
            if !found {
                return Err(errors::TypingError::StructLiteralFieldsMismatch {
                    struct_name: literal_struct.name.clone(),
                    struct_definition_name: type_field.name.clone(),
                });
            }
            fields.push((type_field.name.clone(), field_expression.unwrap()));
        }
        Ok((
            ast::LiteralStruct {
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
                    println!("{:?}\n{:?}", &function_call, &fn_type);
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
            NamedEntity::TypeDeclaration(_) => {
                panic!("Using types not yet supported");
            }
            NamedEntity::Variable(variable) => {
                substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &variable_usage.type_, &variable)?)?;
            }
        }
        Ok((
            ast::VariableUsage {
                name: variable_usage.name.clone(),
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

        let field_type = match get_attr(ctx, &source.type_, &struct_getter.attribute)? {
            StructAttr::Field(type_) => type_,
            StructAttr::Method(type_) => type_,
        };

        substitution = compose_substitutions(ctx, &substitution, &unify(ctx, &struct_getter.type_, &field_type)?)?;

        Ok((
            ast::StructGetter {
                source: source,
                attribute: struct_getter.attribute.clone(),
                type_: apply_substitution(ctx, &substitution, &struct_getter.type_)?,
            },
            substitution,
        ))
    }

    fn with_block_expression(self: &Self, ctx: &Context, substitution: &SubstitutionMap, block: &ast::Block) -> Result<(ast::Block, SubstitutionMap)> {
        let mut substitution = substitution.clone();
        let (result, subst) = self.with_block(ctx, &substitution, &block)?;
        substitution = compose_substitutions(ctx, &substitution, &subst)?;
        Ok((result, substitution))
    }

    fn with_op(self: &Self, ctx: &Context, substitution: &SubstitutionMap, op: &ast::Operation) -> Result<(ast::Operation, SubstitutionMap)> {
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
