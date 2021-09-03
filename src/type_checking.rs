use std::collections::HashMap;
use crate::ast;


pub type SubstitutionMap = HashMap<String, ast::TypeUsage>;

pub enum NamedEntity {
    TypeDeclaration(ast::TypeDeclaration),
    Variable(ast::TypeUsage),
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context {
    pub current_function_return: Option<ast::TypeUsage>,
    pub environment: HashMap<String, ast::NamedEntity>,
}

impl Context {
    fn add_variable(&self, name: String, type_usage: &ast::TypeUsage) -> Context {
        let mut ctx = self.clone();
        ctx.environment[name] = NamedEntity::Variable(type_usage.clone());
        return ctx;
    }

    fn add_type(&self, name: String, type_decl: &ast::TypeDeclaration) -> Context {
        let mut ctx = self.clone();
        ctx.environment[name] = NamedEntity::TypeDeclaration(type_decl.clone());
        return ctx;
    }

    fn set_current_function_return(&self, function: ast::TypeUsage) -> Context {
        let mut ctx = self.clone();
        ctx.current_function_return = Some(function);
        return ctx;
    }
}

fn apply_substitution(substitution: &SubstitutionMap, type_: &ast::TypeUsage) -> ast::TypeUsage {
    match type_ {
        ast::TypeUsage::Named(named) => ast::TypeUsage::Named(named.clone()),
        ast::TypeUsage::Unknown(unknown) => {
            if substitution.contains_key(unknown.name) {
                ast::TypeUsage::Unknown(substitution[unknown.name].clone())
            } else {
                ast::TypeUsage::Unknown(unknown.clone())
            }
        },
        ast::TypeUsage::Function(function) => {
            ast::TypeUsage::Function(FunctionTypeUsage{
                arguments: function.arguments.iter().map(|arg| {
                    apply_substitution(substitution, arg)
                }).collect(),
                return_type: apply_substitution(substitution, function.return_type),
            })
        }
    }
}

fn compose_substitutions(s1: SubstitutionMap, s2: SubstitutionMap) -> SubstitutionMap {
    let mut result = SubstitutionMap::new();
    for k in s2.keys() {
        result[k] = apply_substitution(s1, s2[k]);
    }
    return s1.into_iter().chain(result).collect();
}

fn unify(t1: ast::TypeUsage, t2: ast::TypeUsage) -> SubstitutionMap {
    match (t1, t2) {
        (ast::TypeUsage::Named(named1), ast::TypeUsage::Named(named2)) => {
            if named1.name.name.value == named2.name.name.value {
                return SubstitutionMap::new()
            }
        },
        _ => {},
    }
    if let ast::TypeUsage::Unknown(unknown) = t1 {
        return var_bind(unknown.name, t2);
    }
    if let ast::TypeUsage::Unknown(unknown) = t2 {
        return var_bind(unknown.name, t1);
    }
    match (t1, t2) {
        (ast::TypeUsage::Function(f1), ast::TypeUsage::Function(f2)) => {
            let mut result = unify(f1.return_type, f2.return_type);
            if f1.arguments.len() != f2.arguments.len() {
                panic!("Argument lengths don't match");
            }
            for (i, _) in f1.arguments.iter().enumerate() {
                result = compose_substitutions(result, unify(apply_substitution(result, f1.arguments[i]), apply_substitution(result, f2.arguments[i])));
            }
            return result;
        },
        _ => {},
    }
    panic!("Mismatched unification types");
}

fn var_bind(name: &str, t: ast::TypeUsage) -> SubstitutionMap {
    if let ast::TypeUsage::Unknown(unknown) = t && name == unknown.name {
        return SubstitutionMap::new();
    }
    if contains(t, name) {
        panic!("Type contains a reference to itself")
    }
    let mut substitution = SubstitutionMap::new();
    substitution[name] = t;
    return substitution;
}

fn contains(t: ast::TypeUsage, name: &str) -> bool {
    match t {
        ast::TypeUsage::Named(_) => {
            return false
        },
        ast::TypeUsage::Unknown(unknown) => {
            unknown.name == name
        },
        ast::TypeUsage::Function(f) => {
            if contains(f.return_type, name) {
                return true;
            }
            for arg in f.arguments.iter() {
                if contains(arg, name) {
                    return true;
                }
            }
            return false;
        },
    }
}


pub struct TypeChecker {}

impl TypeChecker {
    pub fn with_module(self: &Self, module: &ast::Module) -> (ast::Module, SubstitutionMap) {
        let mut ctx = Context{
            environment: HashMap::new(), //TODO: builtins
        };

        for item in module.items.iter() {
            match item {
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Struct(struct_)) => {
                    ctx.declarations.push(ast::NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(struct_.clone())));
                },
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Alias(alias)) => {
                    ctx.declarations.push(ast::NamedEntity::TypeDeclaration(ast::TypeDeclaration::Alias(alias.clone())));
                },
                _ => {},
            }
        }

        return ast::Module{
            items: module.items.iter().map(|item|{
                match item {
                    ast::ModuleItem::Function(function) => {
                        ast::ModuleItem::Function(self.with_function(&ctx, function))
                    },
                    ast::ModuleItem::TypeDeclaration(type_declaration) => {
                        ast::ModuleItem::TypeDeclaration(self.with_type_declaration(&ctx, type_declaration))
                    },
                    ast::ModuleItem::Impl(impl_) => {
                        ast::ModuleItem::Impl(self.with_impl(&ctx, impl_))
                    },
                }
            }).collect()
        };
    }

    fn with_function(self: &Self, ctx: &Context, function: &ast::Function) -> (ast::Function, SubstitutionMap) {
        // add args to env
        let mut function_ctx = ctx.set_current_function_return(function.declaration.return_type.clone());
        for arg in function.declaration.arguments.iter() {
            function_ctx = function_ctx.add_variable(arg.name.to_string(), arg.type_.clone());
        }

        let (block, substitution) = self.with_block(function_ctx, &function.block);
        let substitution = unify(block.type_, function.declaration.return_type);

        return (ast::Function{
            declaration: ast::FunctionDeclaration{
                name: function.declaration.name.clone(),
                arguments: function.declaration.arguments.iter().map(|arg| {
                    arg.clone()
                }).collect(),
                return_type: function.declaration.return_type.clone(),
            },
            block: block,
        }, substitution);
    }

    fn with_type_declaration(self: &Self, ctx: &Context, type_declaration: &ast::TypeDeclaration) -> (ast::TypeDeclaration, SubstitutionMap) {
        match type_declaration {
            ast::TypeDeclaration::Struct(struct_) => {
                let (result, substitution) = self.with_struct_declaration(ctx, struct_);
                return (ast::TypeDeclaration::Struct(result), substitution);
            },
            ast::TypeDeclaration::Primitive(primitive) => {
                return (ast::TypeDeclaration::Primitive(primitive.clone()), SubstitutionMap::new());
            },
            ast::TypeDeclaration::Alias(alias) => {
                return (ast::TypeDeclaration::Alias(alias.clone()), SubstitutionMap::new());
            },
        }
    }

    fn with_struct_declaration(self: &Self, ctx: &Context, struct_: &ast::StructTypeDeclaration) -> ast::StructTypeDeclaration {
        return ast::StructTypeDeclaration{
            name: struct_.name.clone(),
            fields: struct_.fields.iter().map(|field|{
                ast::StructField{
                    name: field.name.clone(),
                    type_: field.type_.clone(),
                }
            }).collect(),
        };
    }

    fn with_impl(self: &Self, ctx: &Context, impl_: &ast::Impl) -> (ast::Impl, SubstitutionMap) {
        let mut substitutions = SubstitutionMap::new();
        return (ast::Impl{
            struct_name: impl_.struct_name.clone(),
            functions: impl_.functions.iter().map(|f|{
                let (result, function_subs) = self.with_function(&ctx, f);
                substitutions = compose_substitutions(substitutions, function_subs)
                result
            }).collect(),
        }, substitutions);
    }

    fn with_block(self: &Self, ctx: &Context, block: &ast::Block) -> (ast::Block, SubstitutionMap) {
        let mut substitutions = SubstitutionMap::new();
        let mut block_ctx = ctx.clone();
        return ast::Block{
            statements: block.statements.iter().map(|s| {
                let (statement_ctx, result, statement_substitutions) = self.with_statement(block_ctx, s);
                block_ctx = statement_ctx
                substitutions = compose_substitutions(substitutions, statement_substitutions);
                result
            }).collect(),
            type_: block.type_.clone(),
        };
    }

    fn with_statement(self: &Self, ctx: &Context, statement: &ast::Statement) -> (Context, ast::Statement, SubstitutionMap) {

        match statement {
            ast::Statement::Return(return_statement) => {
                let (result, subst) = self.with_return_statement(ctx, return_statement);
                return (ctx.clone(), ast::Statement::Return(result), subst);
            },
            ast::Statement::Let(let_statement) => {
                let (let_ctx, result, subst) = self.with_let_statement(ctx, let_statement);
                return (let_ctx, ast::Statement::Let(result), subst);
            },
            ast::Statement::Assignment(assignment_statement) => {
                let (result, subst) = self.with_assignment_statement(ctx, assignment_statement);
                return (ctx.clone(), ast::Statement::Assignment(result), subst);
            },
            ast::Statement::Expression(expression) => {
                let (result, subst) = self.with_expression(ctx, expression);
                return (ctx.clone(), ast::Statement::Expression(result), subst);
            },
        }
    }

    fn with_return_statement(self: &Self, ctx: &Context, statement: &ast::ReturnStatement) -> (ast::ReturnStatement, SubstitutionMap) {
        let (result, subst) = self.with_expression(ctx, &statement.source);
        let substitution = compose_substitutions(subst, unify(result.type_, ctx.current_function_return));
        return (ast::ReturnStatement{
            source: result,
        }, substitution);
    }

    fn with_let_statement(self: &Self, ctx: &Context, statement: &ast::LetStatement) -> (Context, ast::LetStatement, SubstitutionMap) {
        let (result, subst) = self.with_expression(ctx, &statement.expression);
        let let_ctx = ctx.add_variable(statement.variable_name.clone(), result.type_);
        let substitution = compose_substitutions(subst, unify(&statement.type_, result.type_));
        return (let_ctx, ast::LetStatement{
            variable_name: statement.variable_name.clone(),
            expression: result,
            type_: &statement.type_.clone(),
        }, substitution);
    }

    fn with_assignment_statement(self: &Self, ctx: &Context, statement: &ast::AssignmentStatement) -> ast::AssignmentStatement {

        return ast::AssignmentStatement{
            source: match &statement.source {
                ast::AssignmentTarget::Variable(variable) => {
                    let assignment_subs = compose_substitutions(subs, unify(&expr.type_, &variable.type_));
                    (ast::AssignmentTarget::Variable(ast::VariableUsage{
                        name: variable.name.clone(),
                        type_: &variable.type_.clone(),
                    }), assignment_subs)
                },
                ast::AssignmentTarget::StructAttr(struct_attr) => {
                    // let assignment_subs = compose_substitutions(subs, unify(&expr.type_, &struct_attr.type_));
                    let (expr, subst) = self.with_expression(ctx, &struct_attr.source);

                    (ast::AssignmentTarget::StructAttr(ast::StructGetter{
                        source: expr,
                        attribute: struct_attr.attribute.clone(),
                        type_: &struct_attr.type_.clone(),
                    }), subst)
                },
            },
            expression: expr,
        }
    }

    fn with_expression(self: &Self, ctx: &Context, expression: &ast::Expression) -> (ast::Expression, SubstitutionMap) {
        let mut substitution = SubstitutionMap::new();
        let expr = ast::Expression{
            subexpression: Box::new(match &*expression.subexpression {
                ast::Subexpression::LiteralInt(literal_int) => {
                    ast::Subexpression::LiteralInt(ast::LiteralInt{
                        value: literal_int.value.clone(),
                        type_: literal_int.type_.clone(),
                    })
                },
                ast::Subexpression::LiteralFloat(literal_float) => {
                    ast::Subexpression::LiteralFloat(ast::LiteralFloat{
                        value: literal_float.value.clone(),
                        type_: literal_float.type_.clone(),
                    })
                },
                ast::Subexpression::LiteralStruct(literal_struct) => {
                    ast::Subexpression::LiteralStruct(ast::LiteralStruct{
                        name: literal_struct.name.clone(),
                        fields: literal_struct.fields.iter().map(|field|{

                            // substitution = compose_substitutions(substitution, );
                            (field.0.clone(), self.with_expression(ctx, &field.1))
                        }).collect(),
                        type_: literal_struct.type_.clone(),
                    })
                },
                ast::Subexpression::FunctionCall(function_call) => {
                    ast::Subexpression::FunctionCall(ast::FunctionCall{
                        source: self.with_expression(ctx, &function_call.source),
                        arguments: function_call.arguments.iter().map(|arg| {self.with_expression(ctx, arg)}).collect(),
                        type_: function_call.type_.clone(),
                    })
                },
                ast::Subexpression::VariableUsage(variable_usage) => {
                    match ctx.environment[variable_usage.name] {
                        NamedEntity::TypeDeclaration(_) => {
                            panic!("Using types not yet supported");
                        },
                        NamedEntity::Variable(variable) => {
                            substitution = compose_substitutions(substitution, unify(variable, expression.type_));
                        },
                    }
                    ast::Subexpression::VariableUsage(ast::VariableUsage{
                        name: variable_usage.name.clone(),
                        type_: variable_usage.type_.clone(),
                    })
                },
                ast::Subexpression::StructGetter(struct_getter) => {
                    ast::Subexpression::StructGetter(ast::StructGetter{
                        source: self.with_expression(ctx, &struct_getter.source),
                        attribute: struct_getter.attribute.clone(),
                        type_: struct_getter.type_.clone(),
                    })
                },
                ast::Subexpression::Block(block) => {
                    let (result, substitution) = self.with_block(ctx, &block);
                    substitution = compose_substitutions(substitution, unify(expression.type_, block.type_));
                    ast::Subexpression::Block(result)
                },
                ast::Subexpression::Op(op) => {
                    let expr_left, subst_left = self.with_expression(ctx, &op.left);
                    let expr_right, subst_right = self.with_expression(ctx, &op.right);
                    substitution = compose_substitutions(substitution, subst_left);
                    substitution = compose_substitutions(substitution, subst_right);
                    substitution = compose_substitutions(substitution, unify(expression.type_, expr_left.type_));
                    substitution = compose_substitutions(substitution, unify(expression.type_, expr_right.type_));
                    ast::Subexpression::Op(ast::Operation{
                        left: expr_left,
                        op: op.op.clone(),
                        right: expr_right,
                    })
                },
            }),
            type_: expression.type_.clone(),
        };
        return (expr, substitution);
    }
}
