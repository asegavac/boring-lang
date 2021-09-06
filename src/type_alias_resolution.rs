use crate::ast;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context {
    pub type_aliases: Vec<ast::AliasTypeDeclaration>,
}

fn resolve_type(ctx: &Context, type_: &ast::NamedTypeUsage) -> ast::TypeUsage {
    let mut changed = true;
    let mut result = ast::TypeUsage::Named(type_.clone());
    while changed {
        changed = false;
        let current = &result.clone();
        match current {
            ast::TypeUsage::Named(named) => {
                for alias in ctx.type_aliases.iter() {
                    if named.name.name.value == alias.name.name.value {
                        changed = true;
                        result = alias.replaces.clone();
                    }
                }
            },
            _ => break,
        }
    }
    return result;
}

fn process_type(ctx: &Context, type_: &ast::TypeUsage) -> ast::TypeUsage {
    match type_ {
        ast::TypeUsage::Named(named) => {
            return resolve_type(ctx, named);
        },
        ast::TypeUsage::Function(function) => {
            return ast::TypeUsage::Function(ast::FunctionTypeUsage{
                arguments: function.arguments.iter().map(|a|{process_type(ctx, &a.clone())}).collect(),
                return_type: Box::new(process_type(ctx, &function.return_type.clone())),
            });
        },
        ast::TypeUsage::Unknown(unknown) => {
            return ast::TypeUsage::Unknown(unknown.clone());
        },
    }
}

pub struct TypeAliasResolver {}

impl TypeAliasResolver {
    pub fn with_module(self: &Self, module: &ast::Module) -> ast::Module {
        let mut ctx = Context{
            type_aliases: vec!(),
        };
        for item in module.items.iter() {
            match item {
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Alias(alias)) => {
                    ctx.type_aliases.push(alias.clone());
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

    fn with_function(self: &Self, ctx: &Context, function: &ast::Function) -> ast::Function {
        return ast::Function{
            declaration: ast::FunctionDeclaration{
                name: function.declaration.name.clone(),
                arguments: function.declaration.arguments.iter().map(|arg| {
                    ast::VariableDeclaration{name: arg.name.clone(), type_: process_type(ctx, &arg.type_)}
                }).collect(),
                return_type: process_type(ctx, &function.declaration.return_type),
            },
            block: self.with_block(ctx, &function.block),
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
                    let result = resolve_type(ctx, &ast::NamedTypeUsage{name: literal_struct.name.clone()});
                    let new_name = match &result {
                        ast::TypeUsage::Named(named) => { named.name.clone() },
                        _ => panic!("LiteralStruct resolved to non-named-type"),
                    };
                    ast::Subexpression::LiteralStruct(ast::LiteralStruct{
                        name: new_name.clone(),
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
