use std::collections::HashMap;
use crate::ast;


type SubstitutionMap = HashMap<String, ast::TypeUsage>;

pub enum NamedEntity {
    Types(ast::TypeDeclaration),
    Values(ast::Value),
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context {
    pub environment: HashMap<String, ast::NamedEntity>,
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
    pub fn with_module(self: &Self, module: &ast::Module) -> ast::Module {
        let mut ctx = Context{
            environment: HashMap::new(), //TODO: builtins
        };

        for item in module.items.iter() {
            match item {
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Struct(struct_)) => {
                    ctx.declarations.push(ast::NamedEntity::Types(ast::TypeDeclaration::Struct(struct_.clone())));
                },
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Alias(alias)) => {
                    ctx.declarations.push(ast::NamedEntity::Types(ast::TypeDeclaration::Alias(alias.clone())));
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

        let (block, substitution) = self.with_block(ctx, &function.block);
        // if block.type_ is not never
        let substitution = unify(block.type_, function.declaration.return_type);

        return ast::Function{
            declaration: ast::FunctionDeclaration{
                name: function.declaration.name.clone(),
                arguments: function.declaration.arguments.iter().map(|arg| {
                    ast::VariableDeclaration{name: arg.name.clone(), type_: process_type(ctx, &arg.type_)}
                }).collect(),
                return_type: apply_substitution(substitution, function.declaration.return_type),
            },
            block: block,
        };
    }

    fn with_type_declaration(self: &Self, ctx: &Context, type_declaration: &ast::TypeDeclaration) -> ast::TypeDeclaration {
        match type_declaration {
            ast::TypeDeclaration::Struct(struct_) => {
                return ast::TypeDeclaration::Struct(self.with_struct_declaration(ctx, struct_));
            },
            ast::TypeDeclaration::Primitive(primitive) => {
                return ast::TypeDeclaration::Primitive(primitive.clone());
            },
            ast::TypeDeclaration::Alias(alias) => {
                return ast::TypeDeclaration::Alias(alias.clone());
            },
        }
    }

    fn with_struct_declaration(self: &Self, ctx: &Context, struct_: &ast::StructTypeDeclaration) -> ast::StructTypeDeclaration {
        return ast::StructTypeDeclaration{
            name: struct_.name.clone(),
            fields: struct_.fields.iter().map(|field|{
                ast::StructField{
                    name: field.name.clone(),
                    type_: process_type(ctx, &field.type_),
                }
            }).collect(),
        };
    }

    fn with_impl(self: &Self, ctx: &Context, impl_: &ast::Impl) -> ast::Impl {
        let mut impl_ctx = ctx.clone();
        impl_ctx.type_aliases.push(ast::AliasTypeDeclaration{
            name: ast::Identifier{
                name: ast::Spanned{
                    span: ast::Span{left: 0, right: 0}, //todo: figure out a sane value for these
                    value:  "Self".to_string(),
                }
            },
            replaces: ast::TypeUsage::Named(ast::NamedTypeUsage{name: impl_.struct_name.clone()})
        });
        return ast::Impl{
            struct_name: impl_.struct_name.clone(),
            functions: impl_.functions.iter().map(|f|{
                self.with_function(&impl_ctx, f)
            }).collect(),
        };
    }

    fn with_block(self: &Self, ctx: &Context, block: &ast::Block) -> ast::Block {
        return ast::Block{
            statements: block.statements.iter().map(|s| {
                self.with_statement(ctx, s)
            }).collect(),
            type_: process_type(ctx, &block.type_),
        };
    }

    fn with_statement(self: &Self, ctx: &Context, statement: &ast::Statement) -> ast::Statement {
        match statement {
            ast::Statement::Return(return_statement) => {
                return ast::Statement::Return(self.with_return_statement(ctx, return_statement));
            },
            ast::Statement::Let(let_statement) => {
                return ast::Statement::Let(self.with_let_statement(ctx, let_statement));
            },
            ast::Statement::Assignment(assignment_statement) => {
                return ast::Statement::Assignment(self.with_assignment_statement(ctx, assignment_statement));
            },
            ast::Statement::Expression(expression) => {
                return ast::Statement::Expression(self.with_expression(ctx, expression));
            },
        }
    }

    fn with_return_statement(self: &Self, ctx: &Context, statement: &ast::ReturnStatement) -> ast::ReturnStatement {
        return ast::ReturnStatement{
            source: self.with_expression(ctx, &statement.source),
        };
    }

    fn with_let_statement(self: &Self, ctx: &Context, statement: &ast::LetStatement) -> ast::LetStatement {
        return ast::LetStatement{
            variable_name: statement.variable_name.clone(),
            expression: self.with_expression(ctx, &statement.expression),
            type_: process_type(ctx, &statement.type_),
        };
    }

    fn with_assignment_statement(self: &Self, ctx: &Context, statement: &ast::AssignmentStatement) -> ast::AssignmentStatement {
        return ast::AssignmentStatement{
            source: match &statement.source {
                ast::AssignmentTarget::Variable(variable) => {
                    ast::AssignmentTarget::Variable(ast::VariableUsage{
                        name: variable.name.clone(),
                        type_: process_type(ctx, &variable.type_),
                    })
                },
                ast::AssignmentTarget::StructAttr(struct_attr) => {
                    ast::AssignmentTarget::StructAttr(ast::StructGetter{
                        source: self.with_expression(ctx, &struct_attr.source),
                        attribute: struct_attr.attribute.clone(),
                        type_: process_type(ctx, &struct_attr.type_)
                    })
                },
            },
            expression: self.with_expression(ctx, &statement.expression),
        }
    }

    fn with_expression(self: &Self, ctx: &Context, expression: &ast::Expression) -> ast::Expression {
        return ast::Expression{
            subexpression: Box::new(match &*expression.subexpression {
                ast::Subexpression::LiteralInt(literal_int) => {
                    ast::Subexpression::LiteralInt(ast::LiteralInt{
                        value: literal_int.value.clone(),
                        type_: process_type(ctx, &literal_int.type_),
                    })
                },
                ast::Subexpression::LiteralFloat(literal_float) => {
                    ast::Subexpression::LiteralFloat(ast::LiteralFloat{
                        value: literal_float.value.clone(),
                        type_: process_type(ctx, &literal_float.type_),
                    })
                },
                ast::Subexpression::LiteralStruct(literal_struct) => {
                    ast::Subexpression::LiteralStruct(ast::LiteralStruct{
                        name: literal_struct.name.clone(),
                        fields: literal_struct.fields.iter().map(|field|{
                            (field.0.clone(), self.with_expression(ctx, &field.1))
                        }).collect(),
                        type_: process_type(ctx, &literal_struct.type_),
                    })
                },
                ast::Subexpression::FunctionCall(function_call) => {
                    ast::Subexpression::FunctionCall(ast::FunctionCall{
                        source: self.with_expression(ctx, &function_call.source),
                        arguments: function_call.arguments.iter().map(|arg| {self.with_expression(ctx, arg)}).collect(),
                        type_: process_type(ctx, &function_call.type_)
                    })
                },
                ast::Subexpression::VariableUsage(variable_usage) => {
                    ast::Subexpression::VariableUsage(ast::VariableUsage{
                        name: variable_usage.name.clone(),
                        type_: process_type(ctx, &variable_usage.type_)
                    })
                },
                ast::Subexpression::StructGetter(struct_getter) => {
                    ast::Subexpression::StructGetter(ast::StructGetter{
                        source: self.with_expression(ctx, &struct_getter.source),
                        attribute: struct_getter.attribute.clone(),
                        type_: process_type(ctx, &struct_getter.type_),
                    })
                },
                ast::Subexpression::Block(block) => {
                    ast::Subexpression::Block(self.with_block(ctx, &block))
                },
                ast::Subexpression::Op(op) => {
                    ast::Subexpression::Op(ast::Operation{
                        left: self.with_expression(ctx, &op.left),
                        op: op.op.clone(),
                        right: self.with_expression(ctx, &op.right),
                    })
                },
            }),
            type_: process_type(ctx, &expression.type_),
        }
    }
}
