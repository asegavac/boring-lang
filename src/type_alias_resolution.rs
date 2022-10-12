use crate::ast;
use crate::errors;

pub type Result<T, E = errors::TypingError> = std::result::Result<T, E>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Context {
    pub type_aliases: Vec<ast::AliasTypeDeclaration>,
}

fn resolve_type(ctx: &Context, type_: &ast::NamedTypeUsage) -> Result<ast::TypeUsage> {
    let mut changed = true;
    let mut result = ast::TypeUsage::Named(type_.clone());
    while changed {
        changed = false;
        let current = &result.clone();
        match current {
            ast::TypeUsage::Named(named) => {
                for alias in ctx.type_aliases.iter() {
                    if named.name.name.value == alias.name.name.value { // is alias, replace
                        changed = true;
                        result = alias.replaces.clone();
                    }
                }
            }
            _ => break,
        }
    }
    match &result {
        ast::TypeUsage::Named(named) => {
            match &named.type_parameters {
                ast::GenericUsage::Known(known) => {
                    let mut result_params = vec!();
                    for param in known.parameters.iter() {
                        result_params.push(process_type(ctx, param)?);
                    }
                    let mut new_named = named.clone();
                    new_named.type_parameters = ast::GenericUsage::new(&result_params);
                    result = ast::TypeUsage::Named(new_named);
                },
                _ => {}
            }
        },
        ast::TypeUsage::Function(func) => {
            match &type_.type_parameters {
                ast::GenericUsage::Known(known) => {
                    if known.parameters.len() > 0 {
                        return Err(errors::TypingError::InvalidTypeParameterOnAlias{alias: type_.name.clone()});
                    }
                },
                _ => {} //skip
            }
        },
        _ => {
            panic!("alias of a non-type, not possible");
        }
    }
    return Ok(result);
}

fn process_type(ctx: &Context, type_: &ast::TypeUsage) -> Result<ast::TypeUsage> {
    match type_ {
        ast::TypeUsage::Named(named) => {
            return Ok(resolve_type(ctx, named)?);
        }
        ast::TypeUsage::Function(function) => {
            let mut arguments = vec!();
            for a in function.arguments.iter() {
                arguments.push(process_type(ctx, &a.clone())?);
            }
            return Ok(ast::TypeUsage::Function(ast::FunctionTypeUsage {
                arguments: arguments,
                return_type: Box::new(process_type(ctx, &function.return_type.clone())?),
            }));
        }
        ast::TypeUsage::Unknown(unknown) => {
            return Ok(ast::TypeUsage::Unknown(unknown.clone()));
        },
        ast::TypeUsage::Namespace(namespace) => {
            match namespace {
                ast::NamespaceTypeUsage::Type(named_type)=> {
                    let result = resolve_type(ctx, named_type)?;
                    match result {
                        ast::TypeUsage::Named(named) => {
                            return Ok(ast::TypeUsage::Namespace(ast::NamespaceTypeUsage::Type(named)));
                        },
                        _ => {
                            return Err(errors::TypingError::InvalidUseofAlias);
                        }
                    }
                }
            }
        }
    }
}

pub struct TypeAliasResolver {}

impl TypeAliasResolver {
    pub fn with_module(self: &Self, module: &ast::Module) -> Result<ast::Module> {
        let mut ctx = Context { type_aliases: vec![] };
        for item in module.items.iter() {
            match item {
                ast::ModuleItem::TypeDeclaration(ast::TypeDeclaration::Alias(alias)) => {
                    ctx.type_aliases.push(alias.clone());
                }
                _ => {}
            }
        }

        let mut items = vec!();
        for item in module.items.iter() {
            items.push(match item {
                ast::ModuleItem::Function(function) => ast::ModuleItem::Function(self.with_function(&ctx, function)?),
                ast::ModuleItem::TypeDeclaration(type_declaration) => {
                    ast::ModuleItem::TypeDeclaration(self.with_type_declaration(&ctx, type_declaration)?)
                }
                ast::ModuleItem::Impl(impl_) => ast::ModuleItem::Impl(self.with_impl(&ctx, impl_)?),
            });
        }

        return Ok(ast::Module {
            items: items,
        });
    }

    fn with_function(self: &Self, ctx: &Context, function: &ast::Function) -> Result<ast::Function> {
        return Ok(ast::Function {
            declaration: self.with_function_declaration(ctx, &function.declaration)?,
            block: self.with_block(ctx, &function.block)?,
        });
    }

    fn with_function_declaration(self: &Self, ctx: &Context, declaration: &ast::FunctionDeclaration) -> Result<ast::FunctionDeclaration> {
        let mut arguments = vec!();
        for arg in declaration.arguments.iter() {
            arguments.push(ast::VariableDeclaration {
                name: arg.name.clone(),
                type_: process_type(ctx, &arg.type_)?,
            });
        }
        return Ok(ast::FunctionDeclaration {
            name: declaration.name.clone(),
            generic: declaration.generic.clone(),
            arguments: arguments,
            return_type: process_type(ctx, &declaration.return_type)?,
        });
    }

    fn with_type_declaration(self: &Self, ctx: &Context, type_declaration: &ast::TypeDeclaration) -> Result<ast::TypeDeclaration> {
        match type_declaration {
            ast::TypeDeclaration::Struct(struct_) => {
                return Ok(ast::TypeDeclaration::Struct(self.with_struct_declaration(ctx, struct_)?));
            }
            ast::TypeDeclaration::Primitive(primitive) => {
                return Ok(ast::TypeDeclaration::Primitive(primitive.clone()));
            }
            ast::TypeDeclaration::Alias(alias) => {
                return Ok(ast::TypeDeclaration::Alias(alias.clone()));
            }
            ast::TypeDeclaration::Trait(trait_) => {
                return Ok(ast::TypeDeclaration::Trait(self.with_trait(ctx, trait_)?));
            }
        }
    }

    fn with_struct_declaration(self: &Self, ctx: &Context, struct_: &ast::StructTypeDeclaration) -> Result<ast::StructTypeDeclaration> {
        let mut fields = vec!();
        for field in struct_.fields.iter() {
            fields.push(ast::StructField {
                name: field.name.clone(),
                type_: process_type(ctx, &field.type_)?,
            });
        }
        return Ok(ast::StructTypeDeclaration {
            generic: struct_.generic.clone(),
            name: struct_.name.clone(),
            fields: fields,
        });
    }

    fn with_trait(self: &Self, ctx: &Context, trait_: &ast::TraitTypeDeclaration) -> Result<ast::TraitTypeDeclaration> {
        let mut trait_ctx = ctx.clone();
        trait_ctx.type_aliases.push(ast::AliasTypeDeclaration {
            name: ast::Identifier {
                name: ast::Spanned {
                    span: ast::Span { left: 0, right: 0 }, //todo: figure out a sane value for these
                    value: "Self".to_string(),
                },
            },
            replaces: ast::TypeUsage::Named(ast::NamedTypeUsage {
                type_parameters: ast::GenericUsage::Unknown,
                name: trait_.name.clone(),
            }),
        });
        let mut functions = vec!();
        for f in trait_.functions.iter() {
            functions.push(match f {
                ast::TraitItem::Function(function) => ast::TraitItem::Function(self.with_function(&trait_ctx, function)?),
                ast::TraitItem::FunctionDeclaration(function_declaration) => {
                    ast::TraitItem::FunctionDeclaration(self.with_function_declaration(&trait_ctx, function_declaration)?)
                }
            });
        }
        return Ok(ast::TraitTypeDeclaration {
            generic: trait_.generic.clone(),
            name: trait_.name.clone(),
            functions: functions,
        });
    }

    fn with_impl(self: &Self, ctx: &Context, impl_: &ast::Impl) -> Result<ast::Impl> {
        let mut impl_ctx = ctx.clone();
        impl_ctx.type_aliases.push(ast::AliasTypeDeclaration {
            name: ast::Identifier {
                name: ast::Spanned {
                    span: ast::Span { left: 0, right: 0 }, //todo: figure out a sane value for these
                    value: "Self".to_string(),
                },
            },
            replaces: ast::TypeUsage::Named(impl_.struct_.clone()),
        });
        let mut functions = vec!();
        for f in impl_.functions.iter() {
            functions.push(self.with_function(&impl_ctx, f)?);
        }
        return Ok(ast::Impl {
            generic: impl_.generic.clone(),
            trait_: impl_.trait_.clone(),
            struct_: impl_.struct_.clone(),
            functions: functions,
        });
    }

    fn with_block(self: &Self, ctx: &Context, block: &ast::Block) -> Result<ast::Block> {
        let mut statements = vec!();
        for s in block.statements.iter() {
            statements.push(self.with_statement(ctx, s)?);
        }
        return Ok(ast::Block {
            statements: statements,
            type_: process_type(ctx, &block.type_)?,
        });
    }

    fn with_statement(self: &Self, ctx: &Context, statement: &ast::Statement) -> Result<ast::Statement> {
        match statement {
            ast::Statement::Return(return_statement) => {
                return Ok(ast::Statement::Return(self.with_return_statement(ctx, return_statement)?));
            }
            ast::Statement::Let(let_statement) => {
                return Ok(ast::Statement::Let(self.with_let_statement(ctx, let_statement)?));
            }
            ast::Statement::Assignment(assignment_statement) => {
                return Ok(ast::Statement::Assignment(self.with_assignment_statement(ctx, assignment_statement)?));
            }
            ast::Statement::Expression(expression) => {
                return Ok(ast::Statement::Expression(self.with_expression(ctx, expression)?));
            }
        }
    }

    fn with_return_statement(self: &Self, ctx: &Context, statement: &ast::ReturnStatement) -> Result<ast::ReturnStatement> {
        return Ok(ast::ReturnStatement {
            source: self.with_expression(ctx, &statement.source)?,
        });
    }

    fn with_let_statement(self: &Self, ctx: &Context, statement: &ast::LetStatement) -> Result<ast::LetStatement> {
        return Ok(ast::LetStatement {
            variable_name: statement.variable_name.clone(),
            expression: self.with_expression(ctx, &statement.expression)?,
            type_: process_type(ctx, &statement.type_)?,
        });
    }

    fn with_assignment_statement(self: &Self, ctx: &Context, statement: &ast::AssignmentStatement) -> Result<ast::AssignmentStatement> {
        return Ok(ast::AssignmentStatement {
            source: match &statement.source {
                ast::AssignmentTarget::Variable(variable) => ast::AssignmentTarget::Variable(ast::VariableUsage {
                    type_parameters: variable.type_parameters.clone(),
                    name: variable.name.clone(),
                    type_: process_type(ctx, &variable.type_)?,
                }),
                ast::AssignmentTarget::StructAttr(struct_attr) => ast::AssignmentTarget::StructAttr(ast::StructGetter {
                    type_parameters: struct_attr.type_parameters.clone(),
                    source: self.with_expression(ctx, &struct_attr.source)?,
                    attribute: struct_attr.attribute.clone(),
                    type_: process_type(ctx, &struct_attr.type_)?,
                }),
            },
            expression: self.with_expression(ctx, &statement.expression)?,
        });
    }

    fn with_expression(self: &Self, ctx: &Context, expression: &ast::Expression) -> Result<ast::Expression> {
        return Ok(ast::Expression {
            subexpression: Box::new(match &*expression.subexpression {
                ast::Subexpression::LiteralInt(literal_int) => ast::Subexpression::LiteralInt(ast::LiteralInt {
                    value: literal_int.value.clone(),
                    type_: process_type(ctx, &literal_int.type_)?,
                }),
                ast::Subexpression::LiteralFloat(literal_float) => ast::Subexpression::LiteralFloat(ast::LiteralFloat {
                    value: literal_float.value.clone(),
                    type_: process_type(ctx, &literal_float.type_)?,
                }),
                ast::Subexpression::LiteralBool(literal_bool) => ast::Subexpression::LiteralBool(ast::LiteralBool {
                    value: literal_bool.value.clone(),
                    type_: process_type(ctx, &literal_bool.type_)?,
                }),
                ast::Subexpression::LiteralString(literal_string) => ast::Subexpression::LiteralString(ast::LiteralString {
                    value: literal_string.value.clone(),
                    type_: process_type(ctx, &literal_string.type_)?,
                }),
                ast::Subexpression::LiteralStruct(literal_struct) => {
                    let result = resolve_type(
                        ctx,
                        &ast::NamedTypeUsage {
                            type_parameters: literal_struct.type_parameters.clone(),
                            name: literal_struct.name.clone(),
                        },
                    )?;
                    let new_name = match &result {
                        ast::TypeUsage::Named(named) => named.name.clone(),
                        _ => panic!("LiteralStruct resolved to non-named-type"),
                    };
                    let mut fields = vec!();
                    for field in literal_struct.fields.iter() {
                        fields.push((field.0.clone(), self.with_expression(ctx, &field.1)?));
                    }
                    ast::Subexpression::LiteralStruct(ast::LiteralStruct {
                        type_parameters: literal_struct.type_parameters.clone(),
                        name: new_name.clone(),
                        fields: fields,
                        type_: process_type(ctx, &literal_struct.type_)?,
                    })
                }
                ast::Subexpression::FunctionCall(function_call) => {
                    let mut arguments = vec!();
                    for arg in function_call.arguments.iter() {
                        arguments.push(self.with_expression(ctx, arg)?);
                    }
                    ast::Subexpression::FunctionCall(ast::FunctionCall {
                        source: self.with_expression(ctx, &function_call.source)?,
                        arguments: arguments,
                        type_: process_type(ctx, &function_call.type_)?,
                    })
                },
                ast::Subexpression::VariableUsage(variable_usage) => ast::Subexpression::VariableUsage(ast::VariableUsage {
                    name: variable_usage.name.clone(),
                    type_parameters: variable_usage.type_parameters.clone(),
                    type_: process_type(ctx, &variable_usage.type_)?,
                }),
                ast::Subexpression::If(if_expression) => ast::Subexpression::If(ast::IfExpression {
                    condition: self.with_expression(ctx, &if_expression.condition)?,
                    block: self.with_block(ctx, &if_expression.block)?,
                    else_: match &if_expression.else_ {
                        Some(else_) => Some(self.with_block(ctx, else_)?),
                        None => None,
                    },
                    type_: process_type(ctx, &if_expression.type_)?,
                }),
                ast::Subexpression::StructGetter(struct_getter) => ast::Subexpression::StructGetter(ast::StructGetter {
                    type_parameters: struct_getter.type_parameters.clone(),
                    source: self.with_expression(ctx, &struct_getter.source)?,
                    attribute: struct_getter.attribute.clone(),
                    type_: process_type(ctx, &struct_getter.type_)?,
                }),
                ast::Subexpression::Block(block) => ast::Subexpression::Block(self.with_block(ctx, &block)?),
                ast::Subexpression::Op(op) => ast::Subexpression::Op(ast::Operation {
                    left: self.with_expression(ctx, &op.left)?,
                    op: op.op.clone(),
                    right: self.with_expression(ctx, &op.right)?,
                }),
            }),
            type_: process_type(ctx, &expression.type_)?,
        });
    }
}
