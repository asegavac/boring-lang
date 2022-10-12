use crate::ast;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

#[derive(Debug, Clone)]
pub enum NumericValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    ISize(isize),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    USize(usize),

    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone)]
pub struct StructValue {
    source: ast::StructTypeDeclaration,
    fields: HashMap<String, Value>,
}

type BuiltinFunction = fn(Vec<Value>) -> Value;

#[derive(Debug, Clone)]
pub enum FunctionRef {
    User(ast::Function),
    Builtin(BuiltinFunction),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub partial: Vec<Value>,
    pub ref_: FunctionRef,
}

#[derive(Debug, Clone)]
pub enum Value {
    Numeric(NumericValue),
    Bool(bool),
    String(String),
    Function(Function),
    Struct(Arc<Mutex<StructValue>>),
    Unit,
}

#[derive(Debug, Clone)]
pub enum NamedEntity {
    TypeDeclaration(ast::TypeDeclaration),
    Variable(Value),
}

#[derive(Debug, Clone)]
struct Context {
    pub environment: HashMap<String, NamedEntity>,
    pub impls: HashMap<String, ast::Impl>,
    pub current_module: ast::Module,
}

impl Context {
    fn set_variable(&mut self, name: String, value: &Value) {
        self.environment.insert(name.to_string(), NamedEntity::Variable(value.clone()));
    }

    fn new_env(&self) -> Context {
        return Context::from_module(&self.current_module);
    }

    fn from_module(module: &ast::Module) -> Context {
        let mut ctx = Context {
            environment: create_builtins(),
            impls: HashMap::new(),
            current_module: module.clone(),
        };

        for item in ctx.current_module.items.iter() {
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
                    ctx.environment.insert(
                        function.declaration.name.name.value.to_string(),
                        NamedEntity::Variable(Value::Function(Function {
                            partial: vec![],
                            ref_: FunctionRef::User(function.clone()),
                        })),
                    );
                }
                ast::ModuleItem::Impl(impl_) => {
                    ctx.impls.insert(impl_.struct_.name.name.value.to_string(), impl_.clone());
                }
                _ => {}
            }
        }
        return ctx;
    }
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
            name: "!".to_string(),
        })),
    );
    result.insert(
        "String".to_string(),
        NamedEntity::TypeDeclaration(ast::TypeDeclaration::Primitive(ast::PrimitiveTypeDeclaration {
            name: "String".to_string(),
        })),
    );

    return result;
}

pub enum ExpressionResult {
    Value(Value),
    Return(Value),
}

pub struct TreeWalkInterpreter {}

impl TreeWalkInterpreter {
    pub fn with_module(self: &Self, module: &ast::Module) -> Value {
        let mut ctx = Context::from_module(module);

        let main = match &ctx.environment["main"] {
            NamedEntity::Variable(Value::Function(func)) => match &func.ref_ {
                FunctionRef::User(ref_) => ref_.clone(),
                _ => panic!("main should be a user defined function"),
            },
            _ => panic!("main should be a user defined function"),
        };

        return self.with_function(&mut ctx, &main);
    }

    fn with_function(self: &Self, ctx: &mut Context, function: &ast::Function) -> Value {
        let result = self.with_block(ctx, &function.block);
        return match result {
            ExpressionResult::Value(r) => r,
            ExpressionResult::Return(r) => r,
        };
    }

    fn with_block(self: &Self, ctx: &mut Context, block: &ast::Block) -> ExpressionResult {
        let mut last = ExpressionResult::Value(Value::Unit);
        for statement in block.statements.iter() {
            let result = self.with_statement(ctx, statement);
            match result {
                ExpressionResult::Return(r) => {
                    return ExpressionResult::Return(r);
                }
                ExpressionResult::Value(r) => {
                    last = ExpressionResult::Value(r);
                }
            }
        }
        return last;
    }

    fn with_statement(self: &Self, ctx: &mut Context, statement: &ast::Statement) -> ExpressionResult {
        match statement {
            ast::Statement::Return(return_statement) => {
                let result = match self.with_expression(ctx, &return_statement.source) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };
                return ExpressionResult::Return(result);
            }
            ast::Statement::Let(let_statement) => {
                let result = match self.with_expression(ctx, &let_statement.expression) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };
                ctx.set_variable(let_statement.variable_name.name.value.to_string(), &result);
                return ExpressionResult::Value(Value::Unit);
            }
            ast::Statement::Assignment(assignment_statement) => {
                return self.with_assignment_statement(ctx, assignment_statement);
            }
            ast::Statement::Expression(expression) => {
                return self.with_expression(ctx, expression);
            }
        }
    }

    fn with_assignment_statement(self: &Self, ctx: &mut Context, statement: &ast::AssignmentStatement) -> ExpressionResult {
        let result = match self.with_expression(ctx, &statement.expression) {
            ExpressionResult::Value(r) => r,
            ExpressionResult::Return(r) => {
                return ExpressionResult::Return(r);
            }
        };
        match &statement.source {
            ast::AssignmentTarget::Variable(variable) => {
                ctx.set_variable(variable.name.name.value.to_string(), &result);
            }
            ast::AssignmentTarget::StructAttr(struct_attr) => {
                let mut source = match self.with_expression(ctx, &struct_attr.source) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };
                match &mut source {
                    Value::Struct(s) => {
                        let mut struct_ = s.lock().unwrap();
                        struct_.fields.insert(struct_attr.attribute.name.value.clone(), result);
                    }
                    _ => panic!("set attr on nonstruct, should never happen due to type system"),
                }
            }
        }
        return ExpressionResult::Value(Value::Unit);
    }

    fn with_expression(self: &Self, ctx: &mut Context, expression: &ast::Expression) -> ExpressionResult {
        match &*expression.subexpression {
            ast::Subexpression::LiteralInt(literal_int) => {
                let value: i64 = literal_int.value.value.parse().unwrap();
                return ExpressionResult::Value(Value::Numeric(NumericValue::I64(value)));
            }
            ast::Subexpression::LiteralFloat(literal_float) => {
                let value: f64 = literal_float.value.value.parse().unwrap();
                return ExpressionResult::Value(Value::Numeric(NumericValue::F64(value)));
            }
            ast::Subexpression::LiteralBool(literal_bool) => {
                let value: bool = if &literal_bool.value.value == "true" { true } else { false };
                return ExpressionResult::Value(Value::Bool(value));
            }
            ast::Subexpression::LiteralString(literal_string) => {
                let value: String = literal_string.value.value.to_string();
                return ExpressionResult::Value(Value::String(value));
            }
            ast::Subexpression::LiteralStruct(literal_struct) => {
                let declaration = match &ctx.environment[&literal_struct.name.name.value] {
                    NamedEntity::TypeDeclaration(ast::TypeDeclaration::Struct(declaration)) => declaration.clone(),
                    _ => panic!("not a struct"),
                };

                let mut fields = HashMap::new();
                for field in declaration.fields.iter() {
                    for (field_name, field_expression) in literal_struct.fields.iter() {
                        if field.name.name.value == field_name.name.value {
                            let field_result = match self.with_expression(ctx, field_expression) {
                                ExpressionResult::Value(r) => r,
                                ExpressionResult::Return(r) => {
                                    return ExpressionResult::Return(r);
                                }
                            };
                            fields.insert(field.name.name.value.to_string(), field_result);
                        }
                    }
                }
                return ExpressionResult::Value(Value::Struct(Arc::new(Mutex::new(StructValue {
                    source: declaration.clone(),
                    fields: fields,
                }))));
            }
            ast::Subexpression::FunctionCall(function_call) => {
                let source = match self.with_expression(ctx, &function_call.source) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };
                let mut argument_values = vec![];
                for arg in function_call.arguments.iter() {
                    let argument_value = match self.with_expression(ctx, arg) {
                        ExpressionResult::Value(r) => r,
                        ExpressionResult::Return(r) => {
                            return ExpressionResult::Return(r);
                        }
                    };
                    argument_values.push(argument_value);
                }
                match &source {
                    Value::Function(function) => match &function.ref_ {
                        FunctionRef::User(user_function) => {
                            let mut fn_ctx = ctx.new_env();
                            let mut i = 0;
                            for partial_arg in &function.partial {
                                fn_ctx.set_variable(
                                    user_function.declaration.arguments[i].name.name.value.to_string(),
                                    &partial_arg.clone(),
                                );
                                i = i + 1;
                            }
                            for argument_value in &argument_values {
                                fn_ctx.set_variable(
                                    user_function.declaration.arguments[i].name.name.value.to_string(),
                                    &argument_value.clone(),
                                );
                            }
                            return ExpressionResult::Value(self.with_function(&mut fn_ctx, user_function));
                        }
                        FunctionRef::Builtin(builtin_function) => {
                            let all_values = function
                                .partial
                                .iter()
                                .map(|val| val.clone())
                                .chain(argument_values.into_iter())
                                .collect();
                            return ExpressionResult::Value(builtin_function(all_values));
                        }
                    },
                    _ => panic!("type error: function call source must be a function"),
                }
            }
            ast::Subexpression::VariableUsage(variable_usage) => {
                let variable_value = match &ctx.environment[&variable_usage.name.name.value] {
                    NamedEntity::Variable(v) => v.clone(),
                    _ => panic!("variable lookup of type"),
                };
                return ExpressionResult::Value(variable_value);
            }
            ast::Subexpression::If(if_expression) => {
                let condition = match self.with_expression(ctx, &if_expression.condition) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };

                match &condition {
                    Value::Bool(cond) => {
                        if cond.clone() {
                            return self.with_block(ctx, &if_expression.block);
                        } else {
                            return match &if_expression.else_ {
                                Some(else_) => self.with_block(ctx, else_),
                                None => ExpressionResult::Value(Value::Unit),
                            };
                        }
                    }
                    _ => panic!("TypeError: condition must be bool"),
                }
            }
            ast::Subexpression::StructGetter(struct_getter) => {
                let source = match self.with_expression(ctx, &struct_getter.source) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };
                match &source {
                    Value::Struct(struct_) => {
                        let s = struct_.lock().unwrap();
                        if s.fields.contains_key(&struct_getter.attribute.name.value) {
                            return ExpressionResult::Value(s.fields[&struct_getter.attribute.name.value].clone());
                        }
                        for module_item in &ctx.current_module.items {
                            match module_item {
                                ast::ModuleItem::Impl(impl_) => {
                                    if impl_.struct_.name.name.value == s.source.name.name.value {
                                        for method in &impl_.functions {
                                            if method.declaration.name.name.value == struct_getter.attribute.name.value {
                                                // if first type matches, partial apply self
                                                if method.declaration.arguments.len() > 0 {
                                                    match &method.declaration.arguments[0].type_ {
                                                        ast::TypeUsage::Named(arg_named) => {
                                                            if arg_named.name.name.value == s.source.name.name.value {
                                                                return ExpressionResult::Value(Value::Function(Function {
                                                                    partial: vec![source.clone()],
                                                                    ref_: FunctionRef::User(method.clone()),
                                                                }));
                                                            }
                                                        }
                                                        _ => {}
                                                    }
                                                }
                                                return ExpressionResult::Value(Value::Function(Function {
                                                    partial: vec![],
                                                    ref_: FunctionRef::User(method.clone()),
                                                }));
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        panic!("TypeError: Method not found");
                    }
                    _ => {
                        panic!("TypeError: struct getter used with non-struct");
                    }
                }
            }
            ast::Subexpression::Block(block) => {
                return self.with_block(ctx, block);
            }
            ast::Subexpression::Op(op) => {
                let left = match self.with_expression(ctx, &op.left) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };
                let right = match self.with_expression(ctx, &op.right) {
                    ExpressionResult::Value(r) => r,
                    ExpressionResult::Return(r) => {
                        return ExpressionResult::Return(r);
                    }
                };
                let result = match (&left, &op.op, &right) {
                    //I
                    (Value::Numeric(NumericValue::I8(l)), ast::Operator::Plus, Value::Numeric(NumericValue::I8(r))) => {
                        Value::Numeric(NumericValue::I8(l + r))
                    }
                    (Value::Numeric(NumericValue::I8(l)), ast::Operator::Minus, Value::Numeric(NumericValue::I8(r))) => {
                        Value::Numeric(NumericValue::I8(l - r))
                    }
                    (Value::Numeric(NumericValue::I8(l)), ast::Operator::Mul, Value::Numeric(NumericValue::I8(r))) => {
                        Value::Numeric(NumericValue::I8(l * r))
                    }
                    (Value::Numeric(NumericValue::I8(l)), ast::Operator::Div, Value::Numeric(NumericValue::I8(r))) => {
                        Value::Numeric(NumericValue::I8(l / r))
                    }

                    (Value::Numeric(NumericValue::I16(l)), ast::Operator::Plus, Value::Numeric(NumericValue::I16(r))) => {
                        Value::Numeric(NumericValue::I16(l + r))
                    }
                    (Value::Numeric(NumericValue::I16(l)), ast::Operator::Minus, Value::Numeric(NumericValue::I16(r))) => {
                        Value::Numeric(NumericValue::I16(l - r))
                    }
                    (Value::Numeric(NumericValue::I16(l)), ast::Operator::Mul, Value::Numeric(NumericValue::I16(r))) => {
                        Value::Numeric(NumericValue::I16(l * r))
                    }
                    (Value::Numeric(NumericValue::I16(l)), ast::Operator::Div, Value::Numeric(NumericValue::I16(r))) => {
                        Value::Numeric(NumericValue::I16(l / r))
                    }

                    (Value::Numeric(NumericValue::I32(l)), ast::Operator::Plus, Value::Numeric(NumericValue::I32(r))) => {
                        Value::Numeric(NumericValue::I32(l + r))
                    }
                    (Value::Numeric(NumericValue::I32(l)), ast::Operator::Minus, Value::Numeric(NumericValue::I32(r))) => {
                        Value::Numeric(NumericValue::I32(l - r))
                    }
                    (Value::Numeric(NumericValue::I32(l)), ast::Operator::Mul, Value::Numeric(NumericValue::I32(r))) => {
                        Value::Numeric(NumericValue::I32(l * r))
                    }
                    (Value::Numeric(NumericValue::I32(l)), ast::Operator::Div, Value::Numeric(NumericValue::I32(r))) => {
                        Value::Numeric(NumericValue::I32(l / r))
                    }

                    (Value::Numeric(NumericValue::I64(l)), ast::Operator::Plus, Value::Numeric(NumericValue::I64(r))) => {
                        Value::Numeric(NumericValue::I64(l + r))
                    }
                    (Value::Numeric(NumericValue::I64(l)), ast::Operator::Minus, Value::Numeric(NumericValue::I64(r))) => {
                        Value::Numeric(NumericValue::I64(l - r))
                    }
                    (Value::Numeric(NumericValue::I64(l)), ast::Operator::Mul, Value::Numeric(NumericValue::I64(r))) => {
                        Value::Numeric(NumericValue::I64(l * r))
                    }
                    (Value::Numeric(NumericValue::I64(l)), ast::Operator::Div, Value::Numeric(NumericValue::I64(r))) => {
                        Value::Numeric(NumericValue::I64(l / r))
                    }

                    //U
                    (Value::Numeric(NumericValue::U8(l)), ast::Operator::Plus, Value::Numeric(NumericValue::U8(r))) => {
                        Value::Numeric(NumericValue::U8(l + r))
                    }
                    (Value::Numeric(NumericValue::U8(l)), ast::Operator::Minus, Value::Numeric(NumericValue::U8(r))) => {
                        Value::Numeric(NumericValue::U8(l - r))
                    }
                    (Value::Numeric(NumericValue::U8(l)), ast::Operator::Mul, Value::Numeric(NumericValue::U8(r))) => {
                        Value::Numeric(NumericValue::U8(l * r))
                    }
                    (Value::Numeric(NumericValue::U8(l)), ast::Operator::Div, Value::Numeric(NumericValue::U8(r))) => {
                        Value::Numeric(NumericValue::U8(l / r))
                    }

                    (Value::Numeric(NumericValue::U16(l)), ast::Operator::Plus, Value::Numeric(NumericValue::U16(r))) => {
                        Value::Numeric(NumericValue::U16(l + r))
                    }
                    (Value::Numeric(NumericValue::U16(l)), ast::Operator::Minus, Value::Numeric(NumericValue::U16(r))) => {
                        Value::Numeric(NumericValue::U16(l - r))
                    }
                    (Value::Numeric(NumericValue::U16(l)), ast::Operator::Mul, Value::Numeric(NumericValue::U16(r))) => {
                        Value::Numeric(NumericValue::U16(l * r))
                    }
                    (Value::Numeric(NumericValue::U16(l)), ast::Operator::Div, Value::Numeric(NumericValue::U16(r))) => {
                        Value::Numeric(NumericValue::U16(l / r))
                    }

                    (Value::Numeric(NumericValue::U32(l)), ast::Operator::Plus, Value::Numeric(NumericValue::U32(r))) => {
                        Value::Numeric(NumericValue::U32(l + r))
                    }
                    (Value::Numeric(NumericValue::U32(l)), ast::Operator::Minus, Value::Numeric(NumericValue::U32(r))) => {
                        Value::Numeric(NumericValue::U32(l - r))
                    }
                    (Value::Numeric(NumericValue::U32(l)), ast::Operator::Mul, Value::Numeric(NumericValue::U32(r))) => {
                        Value::Numeric(NumericValue::U32(l * r))
                    }
                    (Value::Numeric(NumericValue::U32(l)), ast::Operator::Div, Value::Numeric(NumericValue::U32(r))) => {
                        Value::Numeric(NumericValue::U32(l / r))
                    }

                    (Value::Numeric(NumericValue::U64(l)), ast::Operator::Plus, Value::Numeric(NumericValue::U64(r))) => {
                        Value::Numeric(NumericValue::U64(l + r))
                    }
                    (Value::Numeric(NumericValue::U64(l)), ast::Operator::Minus, Value::Numeric(NumericValue::U64(r))) => {
                        Value::Numeric(NumericValue::U64(l - r))
                    }
                    (Value::Numeric(NumericValue::U64(l)), ast::Operator::Mul, Value::Numeric(NumericValue::U64(r))) => {
                        Value::Numeric(NumericValue::U64(l * r))
                    }
                    (Value::Numeric(NumericValue::U64(l)), ast::Operator::Div, Value::Numeric(NumericValue::U64(r))) => {
                        Value::Numeric(NumericValue::U64(l / r))
                    }

                    //F
                    (Value::Numeric(NumericValue::F32(l)), ast::Operator::Plus, Value::Numeric(NumericValue::F32(r))) => {
                        Value::Numeric(NumericValue::F32(l + r))
                    }
                    (Value::Numeric(NumericValue::F32(l)), ast::Operator::Minus, Value::Numeric(NumericValue::F32(r))) => {
                        Value::Numeric(NumericValue::F32(l - r))
                    }
                    (Value::Numeric(NumericValue::F32(l)), ast::Operator::Mul, Value::Numeric(NumericValue::F32(r))) => {
                        Value::Numeric(NumericValue::F32(l * r))
                    }
                    (Value::Numeric(NumericValue::F32(l)), ast::Operator::Div, Value::Numeric(NumericValue::F32(r))) => {
                        Value::Numeric(NumericValue::F32(l / r))
                    }

                    (Value::Numeric(NumericValue::F64(l)), ast::Operator::Plus, Value::Numeric(NumericValue::F64(r))) => {
                        Value::Numeric(NumericValue::F64(l + r))
                    }
                    (Value::Numeric(NumericValue::F64(l)), ast::Operator::Minus, Value::Numeric(NumericValue::F64(r))) => {
                        Value::Numeric(NumericValue::F64(l - r))
                    }
                    (Value::Numeric(NumericValue::F64(l)), ast::Operator::Mul, Value::Numeric(NumericValue::F64(r))) => {
                        Value::Numeric(NumericValue::F64(l * r))
                    }
                    (Value::Numeric(NumericValue::F64(l)), ast::Operator::Div, Value::Numeric(NumericValue::F64(r))) => {
                        Value::Numeric(NumericValue::F64(l / r))
                    }

                    //fail
                    _ => panic!(""),
                };
                return ExpressionResult::Value(result);
            }
        }
    }
}
