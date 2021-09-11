use crate::ast;
use std::collections::HashMap;

pub type SubstitutionMap = HashMap<String, ast::TypeUsage>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamedEntity {
    TypeDeclaration(ast::TypeDeclaration),
    Variable(ast::TypeUsage),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    pub current_function_return: Option<ast::TypeUsage>,
    pub impls: HashMap<String, ast::Impl>,
    pub environment: HashMap<String, NamedEntity>,
}

fn create_builtins() -> HashMap<String, NamedEntity> {
    let mut result = HashMap::new();
    result.insert(
        "i8".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "i8".to_string(),
            },
        )),
    );
    result.insert(
        "i16".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "i16".to_string(),
            },
        )),
    );
    result.insert(
        "i32".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "i32".to_string(),
            },
        )),
    );
    result.insert(
        "i64".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "i64".to_string(),
            },
        )),
    );
    result.insert(
        "isize".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "isize".to_string(),
            },
        )),
    );

    result.insert(
        "u8".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "u8".to_string(),
            },
        )),
    );
    result.insert(
        "u16".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "u16".to_string(),
            },
        )),
    );
    result.insert(
        "u32".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "u32".to_string(),
            },
        )),
    );
    result.insert(
        "u64".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "u64".to_string(),
            },
        )),
    );
    result.insert(
        "usize".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "usize".to_string(),
            },
        )),
    );

    result.insert(
        "f32".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "f32".to_string(),
            },
        )),
    );
    result.insert(
        "f64".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "f64".to_string(),
            },
        )),
    );

    result.insert(
        "!".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "!".to_string(),
            },
        )),
    );
    result.insert(
        "unit".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(
            ast::PrimitiveTypeDeclaration {
                name: "!".to_string(),
            },
        )),
    );

    return result;
}

impl Context {
    fn add_variable(&self, name: String, type_usage: &ast::TypeUsage) -> Context {
        let mut ctx = self.clone();
        ctx.environment
            .insert(name.to_string(), NamedEntity::Variable(type_usage.clone()));
        return ctx;
    }

    fn add_type(&self, name: String, type_decl: &ast::TypeDeclaration) -> Context {
        let mut ctx = self.clone();
        ctx.environment.insert(
            name.to_string(),
            NamedEntity::TypeDeclaration(type_decl.clone()),
        );
        return ctx;
    }

    fn set_current_function_return(&self, function: &ast::TypeUsage) -> Context {
        let mut ctx = self.clone();
        ctx.current_function_return = Some(function.clone());
        return ctx;
    }
}

fn type_exists(ctx: &Context, type_: &ast::TypeUsage) {
    // TODO: error handling
    match type_ {
        ast::TypeUsage::Named(named) => {
            if !ctx.environment.contains_key(&named.name.name.value) {
                panic!("unknown type: {}", &named.name.name.value);
            }
            match ctx.environment[&named.name.name.value] {
                NamedEntity::TypeDeclaration(_) => {
                    // is a type
                }
                _ => {
                    panic!("unknown type")
                }
            }
        }
        ast::TypeUsage::Unknown(unknown) => {} // do nothing
        ast::TypeUsage::Function(function) => {
            for arg in function.arguments.iter() {
                type_exists(ctx, arg);
            }
            type_exists(ctx, &function.return_type);
        }
    }
}

fn apply_substitution(
    ctx: &Context,
    substitution: &SubstitutionMap,
    type_: &ast::TypeUsage,
) -> ast::TypeUsage {
    let result = match type_ {
        ast::TypeUsage::Named(named) => ast::TypeUsage::Named(named.clone()),
        ast::TypeUsage::Unknown(unknown) => {
            if substitution.contains_key(&unknown.name) {
                substitution[&unknown.name].clone()
            } else {
                ast::TypeUsage::Unknown(unknown.clone())
            }
        }
        ast::TypeUsage::Function(function) => ast::TypeUsage::Function(ast::FunctionTypeUsage {
            arguments: function
                .arguments
                .iter()
                .map(|arg| apply_substitution(ctx, substitution, arg))
                .collect(),
            return_type: Box::new(apply_substitution(ctx, substitution, &function.return_type)),
        }),
    };
    type_exists(ctx, &result);
    return result;
}

fn compose_substitutions(
    ctx: &Context,
    s1: &SubstitutionMap,
    s2: &SubstitutionMap,
) -> SubstitutionMap {
    let mut result = SubstitutionMap::new();
    for k in s2.keys() {
        result.insert(k.to_string(), apply_substitution(ctx, s1, &s2[k]));
    }
    return s1
        .into_iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .chain(result)
        .collect();
}

fn unify(ctx: &Context, t1: &ast::TypeUsage, t2: &ast::TypeUsage) -> SubstitutionMap {
    match (t1, t2) {
        (ast::TypeUsage::Named(named1), ast::TypeUsage::Named(named2)) => {
            // if named1.name.name.value == "!" || named2.name.name.value == "!" {
            //     return SubstitutionMap::new(); // never matches with everything
            // }
            if named1.name.name.value == named2.name.name.value {
                return SubstitutionMap::new();
            }
        }
        _ => {}
    }
    match t1 {
        ast::TypeUsage::Unknown(unknown) => {
            return var_bind(&unknown.name, t2);
        }
        _ => {}
    }
    match t2 {
        ast::TypeUsage::Unknown(unknown) => {
            return var_bind(&unknown.name, t1);
        }
        _ => {}
    }
    match (t1, t2) {
        (ast::TypeUsage::Function(f1), ast::TypeUsage::Function(f2)) => {
            let mut result = unify(ctx, &*f1.return_type, &*f2.return_type);
            if f1.arguments.len() != f2.arguments.len() {
                panic!("Argument lengths don't match");
            }
            for (i, _) in f1.arguments.iter().enumerate() {
                result = compose_substitutions(
                    ctx,
                    &result,
                    &unify(
                        ctx,
                        &apply_substitution(ctx, &result, &f1.arguments[i]),
                        &apply_substitution(ctx, &result, &f2.arguments[i]),
                    ),
                );
            }
            return result;
        }
        _ => {}
    }
    println!("problem:\n{:?}\n{:?}", t1, t2);
    panic!("Mismatched unification types");
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
    pub fn with_module(self: &Self, module: &ast::Module) -> (ast::Module, SubstitutionMap) {
        let mut ctx = Context {
            environment: create_builtins(),
            impls: HashMap::new(),
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
                ast::ModuleItem::Function(function) => {
                    let function_type = ast::FunctionTypeUsage {
                        arguments: function
                            .declaration
                            .arguments
                            .iter()
                            .map(|arg| arg.type_.clone())
                            .collect(),
                        return_type: Box::new(function.declaration.return_type.clone()),
                    };
                    ctx.environment.insert(
                        function.declaration.name.name.value.to_string(),
                        NamedEntity::Variable(ast::TypeUsage::Function(function_type)),
                    );
                }
                ast::ModuleItem::Impl(impl_) => {
                    ctx.impls
                        .insert(impl_.struct_name.name.value.to_string(), impl_.clone());
                }
                _ => {}
            }
        }

        let mut subst = SubstitutionMap::new();
        let result = ast::Module {
            items: module
                .items
                .iter()
                .map(|item| match item {
                    ast::ModuleItem::Function(function) => {
                        let (func, fn_subst) = self.with_function(&ctx, &subst, function);
                        subst = compose_substitutions(&ctx, &subst, &fn_subst);
                        ast::ModuleItem::Function(func)
                    }
                    ast::ModuleItem::TypeDeclaration(type_declaration) => {
                        let (ty_decl, ty_subst) =
                            self.with_type_declaration(&ctx, type_declaration);
                        subst = compose_substitutions(&ctx, &subst, &ty_subst);
                        ast::ModuleItem::TypeDeclaration(ty_decl)
                    }
                    ast::ModuleItem::Impl(impl_) => {
                        let (impl_result, impl_subst) = self.with_impl(&ctx, &subst, impl_);
                        subst = compose_substitutions(&ctx, &subst, &impl_subst);
                        ast::ModuleItem::Impl(impl_result)
                    }
                })
                .collect(),
        };
        return (result, subst);
    }

    fn with_function(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        function: &ast::Function,
    ) -> (ast::Function, SubstitutionMap) {
        // add args to env
        let mut function_ctx =
            ctx.set_current_function_return(&function.declaration.return_type.clone());
        for arg in function.declaration.arguments.iter() {
            type_exists(ctx, &arg.type_);
            function_ctx =
                function_ctx.add_variable(arg.name.name.value.to_string(), &arg.type_.clone());
        }
        type_exists(ctx, &function.declaration.return_type);

        let (block, substitution) =
            self.with_block(&function_ctx, incoming_substitutions, &function.block);
        let mut substitution =
            compose_substitutions(&function_ctx, incoming_substitutions, &substitution);
        match &block.type_ {
            ast::TypeUsage::Named(named) => {
                if named.name.name.value != "!" {
                    substitution = compose_substitutions(
                        &function_ctx,
                        &substitution,
                        &unify(
                            &function_ctx,
                            &function.declaration.return_type,
                            &block.type_,
                        ),
                    );
                }
            }
            _ => {
                substitution = compose_substitutions(
                    &function_ctx,
                    &substitution,
                    &unify(
                        &function_ctx,
                        &function.declaration.return_type,
                        &block.type_,
                    ),
                );
            }
        }

        return (
            ast::Function {
                declaration: ast::FunctionDeclaration {
                    name: function.declaration.name.clone(),
                    arguments: function
                        .declaration
                        .arguments
                        .iter()
                        .map(|arg| arg.clone())
                        .collect(),
                    return_type: function.declaration.return_type.clone(),
                },
                block: block,
            },
            substitution,
        );
    }

    fn with_type_declaration(
        self: &Self,
        ctx: &Context,
        type_declaration: &ast::TypeDeclaration,
    ) -> (ast::TypeDeclaration, SubstitutionMap) {
        match type_declaration {
            ast::TypeDeclaration::Struct(struct_) => {
                let result = self.with_struct_declaration(ctx, struct_);
                return (ast::TypeDeclaration::Struct(result), SubstitutionMap::new());
            }
            ast::TypeDeclaration::Primitive(primitive) => {
                return (
                    ast::TypeDeclaration::Primitive(primitive.clone()),
                    SubstitutionMap::new(),
                );
            }
            ast::TypeDeclaration::Alias(alias) => {
                return (
                    ast::TypeDeclaration::Alias(alias.clone()),
                    SubstitutionMap::new(),
                );
            }
        }
    }

    fn with_struct_declaration(
        self: &Self,
        ctx: &Context,
        struct_: &ast::StructTypeDeclaration,
    ) -> ast::StructTypeDeclaration {
        return ast::StructTypeDeclaration {
            name: struct_.name.clone(),
            fields: struct_
                .fields
                .iter()
                .map(|field| {
                    type_exists(ctx, &field.type_);
                    ast::StructField {
                        name: field.name.clone(),
                        type_: field.type_.clone(),
                    }
                })
                .collect(),
        };
    }

    fn with_impl(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        impl_: &ast::Impl,
    ) -> (ast::Impl, SubstitutionMap) {
        let mut substitutions = incoming_substitutions.clone();
        type_exists(ctx, &ast::TypeUsage::new_named(impl_.struct_name.clone()));
        return (
            ast::Impl {
                struct_name: impl_.struct_name.clone(),
                functions: impl_
                    .functions
                    .iter()
                    .map(|f| {
                        let (result, function_subs) = self.with_function(&ctx, &substitutions, f);
                        substitutions = compose_substitutions(ctx, &substitutions, &function_subs);
                        result
                    })
                    .collect(),
            },
            substitutions,
        );
    }

    fn with_block(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        block: &ast::Block,
    ) -> (ast::Block, SubstitutionMap) {
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
        let statements = block
            .statements
            .iter()
            .map(|s| {
                let (statement_ctx, result, statement_substitutions) =
                    self.with_statement(&block_ctx, &substitutions, s);
                block_ctx = statement_ctx;
                substitutions =
                    compose_substitutions(&block_ctx, &substitutions, &statement_substitutions);
                result
            })
            .collect();
        if !has_return {
            match block.statements.last().unwrap() {
                ast::Statement::Expression(expr) => {
                    substitutions = compose_substitutions(
                        &block_ctx,
                        &substitutions,
                        &unify(&block_ctx, &block.type_, &expr.type_),
                    );
                }
                _ => {
                    substitutions = compose_substitutions(
                        &block_ctx,
                        &substitutions,
                        &unify(&block_ctx, &block.type_, &ast::new_unit()),
                    );
                }
            }
        }
        let result_type = if has_return {
            ast::new_never()
        } else {
            apply_substitution(&block_ctx, &substitutions, &block.type_)
        };
        let block_result = ast::Block {
            statements: statements,
            type_: result_type,
        };
        return (block_result, substitutions);
    }

    fn with_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::Statement,
    ) -> (Context, ast::Statement, SubstitutionMap) {
        match statement {
            ast::Statement::Return(return_statement) => {
                let (result, subst) =
                    self.with_return_statement(ctx, incoming_substitutions, return_statement);
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst);
                return (ctx.clone(), ast::Statement::Return(result), subst);
            }
            ast::Statement::Let(let_statement) => {
                let (let_ctx, result, subst) =
                    self.with_let_statement(ctx, incoming_substitutions, let_statement);
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst);
                return (let_ctx, ast::Statement::Let(result), subst);
            }
            ast::Statement::Assignment(assignment_statement) => {
                let (result, subst) = self.with_assignment_statement(
                    ctx,
                    incoming_substitutions,
                    assignment_statement,
                );
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst);
                return (ctx.clone(), ast::Statement::Assignment(result), subst);
            }
            ast::Statement::Expression(expression) => {
                let (result, subst) = self.with_expression(ctx, incoming_substitutions, expression);
                let subst = compose_substitutions(ctx, &incoming_substitutions, &subst);
                return (ctx.clone(), ast::Statement::Expression(result), subst);
            }
        }
    }

    fn with_return_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::ReturnStatement,
    ) -> (ast::ReturnStatement, SubstitutionMap) {
        let (result, subst) = self.with_expression(ctx, incoming_substitutions, &statement.source);
        let mut substitution = compose_substitutions(ctx, &incoming_substitutions, &subst);
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
                &unify(
                    ctx,
                    &ctx.current_function_return.as_ref().unwrap(),
                    &result.type_,
                ),
            );
        }

        return (ast::ReturnStatement { source: result }, substitution);
    }

    fn with_let_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::LetStatement,
    ) -> (Context, ast::LetStatement, SubstitutionMap) {
        let (result, subst) =
            self.with_expression(ctx, incoming_substitutions, &statement.expression);
        let let_ctx = ctx.add_variable(statement.variable_name.name.value.clone(), &result.type_);
        let substitution =
            compose_substitutions(ctx, &subst, &unify(ctx, &statement.type_, &result.type_));
        return (
            let_ctx,
            ast::LetStatement {
                variable_name: statement.variable_name.clone(),
                expression: result,
                type_: apply_substitution(ctx, &substitution, &statement.type_),
            },
            substitution,
        );
    }

    fn with_assignment_statement(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        statement: &ast::AssignmentStatement,
    ) -> (ast::AssignmentStatement, SubstitutionMap) {
        let (expr, subst) =
            self.with_expression(ctx, incoming_substitutions, &statement.expression);
        let mut substitution = compose_substitutions(ctx, &incoming_substitutions, &subst);

        let result_as = ast::AssignmentStatement {
            source: match &statement.source {
                ast::AssignmentTarget::Variable(variable) => {
                    substitution = compose_substitutions(
                        ctx,
                        &substitution,
                        &unify(ctx, &variable.type_, &expr.type_),
                    );
                    ast::AssignmentTarget::Variable(ast::VariableUsage {
                        name: variable.name.clone(),
                        type_: apply_substitution(ctx, &substitution, &variable.type_),
                    })
                }
                ast::AssignmentTarget::StructAttr(struct_attr) => {
                    let (source, subst) =
                        self.with_expression(ctx, &substitution, &struct_attr.source);
                    let mut subst = subst.clone();

                    match &source.type_ {
                        ast::TypeUsage::Named(named) => {
                            match &ctx.environment[&named.name.name.value] {
                                NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(
                                    type_declaration,
                                )) => {
                                    let mut found = false;
                                    for field in type_declaration.fields.iter() {
                                        if field.name.name.value == struct_attr.attribute.name.value
                                        {
                                            found = true;
                                            subst = compose_substitutions(
                                                ctx,
                                                &subst,
                                                &unify(ctx, &struct_attr.type_, &field.type_),
                                            );
                                        }
                                    }
                                    if !found {
                                        panic!("unknown field name")
                                    }
                                }
                                _ => panic!("struct getter being used on non-struct"),
                            }
                        }
                        ast::TypeUsage::Function(_) => {
                            panic!("function used with attr")
                        }
                        _ => {} // skip unifying if struct type is unknown1
                    }

                    let substitution = compose_substitutions(
                        ctx,
                        &compose_substitutions(ctx, &substitution, &subst),
                        &unify(ctx, &struct_attr.type_, &expr.type_),
                    );
                    ast::AssignmentTarget::StructAttr(ast::StructGetter {
                        source: source,
                        attribute: struct_attr.attribute.clone(),
                        type_: apply_substitution(ctx, &substitution, &struct_attr.type_),
                    })
                }
            },
            expression: expr,
        };
        return (result_as, substitution);
    }

    fn with_expression(
        self: &Self,
        ctx: &Context,
        incoming_substitutions: &SubstitutionMap,
        expression: &ast::Expression,
    ) -> (ast::Expression, SubstitutionMap) {
        let mut substitution = incoming_substitutions.clone();
        let subexpression = Box::new(match &*expression.subexpression {
            ast::Subexpression::LiteralInt(literal_int) => {
                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &literal_int.type_),
                );
                ast::Subexpression::LiteralInt(ast::LiteralInt {
                    value: literal_int.value.clone(),
                    type_: apply_substitution(ctx, &substitution, &literal_int.type_),
                })
            }
            ast::Subexpression::LiteralFloat(literal_float) => {
                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &literal_float.type_),
                );
                ast::Subexpression::LiteralFloat(ast::LiteralFloat {
                    value: literal_float.value.clone(),
                    type_: apply_substitution(ctx, &substitution, &literal_float.type_),
                })
            }
            ast::Subexpression::LiteralStruct(literal_struct) => {
                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &literal_struct.type_),
                );
                let type_declaration = match &ctx.environment[&literal_struct.name.name.value] {
                    NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(
                        type_declaration,
                    )) => type_declaration,
                    _ => {
                        panic!("literal struct used with non struct name")
                    }
                };
                if type_declaration.fields.len() != literal_struct.fields.len() {
                    panic!("literal type declaration has mismatched fields");
                }
                ast::Subexpression::LiteralStruct(ast::LiteralStruct {
                    name: literal_struct.name.clone(),
                    fields: type_declaration
                        .fields
                        .iter()
                        .map(|type_field| {
                            let mut found = false;
                            let mut field_expression: Option<ast::Expression> = None;
                            for field in literal_struct.fields.iter() {
                                if type_field.name.name.value == field.0.name.value {
                                    found = true;
                                    let (result, subst) =
                                        self.with_expression(ctx, &substitution, &field.1);
                                    substitution =
                                        compose_substitutions(ctx, &substitution, &subst);
                                    substitution = compose_substitutions(
                                        ctx,
                                        &substitution,
                                        &unify(ctx, &type_field.type_, &result.type_),
                                    );
                                    field_expression = Some(result);
                                }
                            }
                            if !found {
                                panic!("missing field: {}", &type_field.name.name.value);
                            }
                            (type_field.name.clone(), field_expression.unwrap())
                        })
                        .collect(),
                    type_: apply_substitution(ctx, &substitution, &literal_struct.type_),
                })
            }
            ast::Subexpression::FunctionCall(function_call) => {
                let (source, subst) =
                    self.with_expression(ctx, &substitution, &function_call.source);
                substitution = compose_substitutions(ctx, &substitution, &subst);
                match &source.type_ {
                    ast::TypeUsage::Function(fn_type) => {
                        substitution = compose_substitutions(
                            ctx,
                            &substitution,
                            &unify(ctx, &function_call.type_, &*fn_type.return_type),
                        );
                        if function_call.arguments.len() != fn_type.arguments.len() {
                            panic!("mismatched function argument count");
                        }
                    }
                    ast::TypeUsage::Named(_) => panic!("FunctionCall doesn't have function type."),
                    _ => {}
                }
                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &function_call.type_),
                );
                ast::Subexpression::FunctionCall(ast::FunctionCall {
                    source: source.clone(),
                    arguments: function_call
                        .arguments
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| {
                            let (result, subst) = self.with_expression(ctx, &substitution, arg);
                            substitution = compose_substitutions(ctx, &substitution, &subst);

                            match &source.type_ {
                                ast::TypeUsage::Function(fn_type) => {
                                    substitution = compose_substitutions(
                                        ctx,
                                        &substitution,
                                        &unify(ctx, &fn_type.arguments[i], &result.type_),
                                    );
                                }
                                ast::TypeUsage::Named(_) => {
                                    panic!("FunctionCall doesn't have function type.")
                                }
                                _ => {}
                            }
                            result
                        })
                        .collect(),
                    type_: apply_substitution(ctx, &substitution, &function_call.type_),
                })
            }
            ast::Subexpression::VariableUsage(variable_usage) => {
                match &ctx.environment[&variable_usage.name.name.value] {
                    NamedEntity::TypeDeclaration(_) => {
                        panic!("Using types not yet supported");
                    }
                    NamedEntity::Variable(variable) => {
                        substitution = compose_substitutions(
                            ctx,
                            &substitution,
                            &unify(ctx, &variable_usage.type_, &variable),
                        );
                        substitution = compose_substitutions(
                            ctx,
                            &substitution,
                            &unify(ctx, &expression.type_, &variable_usage.type_),
                        );
                    }
                }
                ast::Subexpression::VariableUsage(ast::VariableUsage {
                    name: variable_usage.name.clone(),
                    type_: apply_substitution(ctx, &substitution, &variable_usage.type_),
                })
            }
            ast::Subexpression::StructGetter(struct_getter) => {
                let (source, subst) =
                    self.with_expression(ctx, &substitution, &struct_getter.source);
                substitution = compose_substitutions(ctx, &substitution, &subst);

                match &source.type_ {
                    ast::TypeUsage::Named(named) => {
                        match &ctx.environment[&named.name.name.value] {
                            NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(
                                type_declaration,
                            )) => {
                                let mut found = false;
                                for field in type_declaration.fields.iter() {
                                    if field.name.name.value == struct_getter.attribute.name.value {
                                        found = true;
                                        substitution = compose_substitutions(
                                            ctx,
                                            &substitution,
                                            &unify(ctx, &struct_getter.type_, &field.type_),
                                        );
                                    }
                                }
                                if !found {
                                    for method in ctx.impls[&type_declaration.name.name.value]
                                        .functions
                                        .iter()
                                    {
                                        if method.declaration.name.name.value
                                            == struct_getter.attribute.name.value
                                        {
                                            let mut function_type = ast::FunctionTypeUsage {
                                                arguments: method
                                                    .declaration
                                                    .arguments
                                                    .iter()
                                                    .map(|arg| arg.type_.clone())
                                                    .collect(),
                                                return_type: Box::new(
                                                    method.declaration.return_type.clone(),
                                                ),
                                            };
                                            // if the name of the type of the first argument == the class, remove the first arg
                                            if function_type.arguments.len() > 0 {
                                                match &function_type.arguments[0] {
                                                    ast::TypeUsage::Named(named) => {
                                                        if named.name.name.value
                                                            == type_declaration.name.name.value
                                                        {
                                                            function_type =
                                                                ast::FunctionTypeUsage {
                                                                    arguments: method
                                                                        .declaration
                                                                        .arguments
                                                                        [1..method
                                                                            .declaration
                                                                            .arguments
                                                                            .len()]
                                                                        .iter()
                                                                        .map(|arg| {
                                                                            arg.type_.clone()
                                                                        })
                                                                        .collect(),
                                                                    return_type: Box::new(
                                                                        method
                                                                            .declaration
                                                                            .return_type
                                                                            .clone(),
                                                                    ),
                                                                };
                                                        }
                                                    }
                                                    _ => {}
                                                };
                                            }

                                            println!("found: {:?}", &function_type);
                                            substitution = compose_substitutions(
                                                ctx,
                                                &substitution,
                                                &unify(
                                                    ctx,
                                                    &struct_getter.type_,
                                                    &ast::TypeUsage::Function(function_type),
                                                ),
                                            );
                                            found = true;
                                        }
                                    }
                                }
                                if !found {
                                    panic!("unknown field name")
                                }
                            }
                            _ => panic!("struct getter being used on non-struct"),
                        }
                    }
                    ast::TypeUsage::Function(_) => {
                        panic!("function used with attr")
                    }
                    _ => {} // skip unifying if struct type is unknown1
                }

                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &struct_getter.type_),
                );

                ast::Subexpression::StructGetter(ast::StructGetter {
                    source: source,
                    attribute: struct_getter.attribute.clone(),
                    type_: apply_substitution(ctx, &substitution, &struct_getter.type_),
                })
            }
            ast::Subexpression::Block(block) => {
                let (result, subst) = self.with_block(ctx, &substitution, &block);
                substitution = compose_substitutions(ctx, &substitution, &subst);
                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &result.type_),
                );
                ast::Subexpression::Block(result)
            }
            ast::Subexpression::Op(op) => {
                let (expr_left, subst_left) = self.with_expression(ctx, &substitution, &op.left);
                let (expr_right, subst_right) = self.with_expression(ctx, &substitution, &op.right);
                substitution = compose_substitutions(ctx, &substitution, &subst_left);
                substitution = compose_substitutions(ctx, &substitution, &subst_right);
                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &expr_left.type_),
                );
                substitution = compose_substitutions(
                    ctx,
                    &substitution,
                    &unify(ctx, &expression.type_, &expr_right.type_),
                );
                ast::Subexpression::Op(ast::Operation {
                    left: expr_left,
                    op: op.op.clone(),
                    right: expr_right,
                })
            }
        });

        let expr = ast::Expression {
            subexpression: subexpression,
            type_: apply_substitution(ctx, &substitution, &expression.type_),
        };
        return (expr, substitution);
    }
}
