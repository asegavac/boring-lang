use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue};
use std::collections::HashMap;
use std::convert::TryInto;
use std::mem;

use crate::ast;

type Scope<'ctx> = HashMap<String, BasicValueEnum<'ctx>>;

pub struct ModuleCodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    scope: Scope<'ctx>,
}

impl<'ctx> ModuleCodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: String) -> Self {
        return ModuleCodeGen {
            context: context,
            module: context.create_module(&name),
            builder: context.create_builder(),
            scope: Scope::new(),
        };
    }

    pub fn gen_literal_int(&mut self, literal_int: &ast::LiteralInt) -> IntValue<'ctx> {
        self.context.i64_type().const_int(
            unsafe { mem::transmute::<i64, u64>(literal_int.value) },
            true,
        )
    }

    pub fn gen_op_expression(
        &mut self,
        scope: &Scope<'ctx>,
        operation: &ast::Operation,
    ) -> IntValue<'ctx> {
        let lhs_result = self.gen_expression(scope, &operation.left);
        let rhs_result = self.gen_expression(scope, &operation.right);
        self.gen_op_int(&lhs_result, &rhs_result, &operation.op)
    }

    pub fn gen_op_int(
        &mut self,
        lhs: &IntValue<'ctx>,
        rhs: &IntValue<'ctx>,
        op: &ast::Operator,
    ) -> IntValue<'ctx> {
        match *op {
            ast::Operator::Plus => self.builder.build_int_add(*lhs, *rhs, "add"),
            ast::Operator::Minus => self.builder.build_int_sub(*lhs, *rhs, "sub"),
            ast::Operator::Mul => self.builder.build_int_mul(*lhs, *rhs, "mul"),
            ast::Operator::Div => self.builder.build_int_signed_div(*lhs, *rhs, "div"),
        }
    }

    pub fn gen_expression(
        &mut self,
        scope: &Scope<'ctx>,
        expression: &Box<ast::Expression>,
    ) -> IntValue<'ctx> {
        match &**expression {
            ast::Expression::LiteralInt(literal_int) => self.gen_literal_int(&literal_int),
            ast::Expression::Identifier(identifier) => match scope[&identifier.name] {
                BasicValueEnum::IntValue(value) => value,
                _ => panic!("function returned type other than int, no types yet"),
            },
            ast::Expression::FunctionCall(function_call) => {
                self.gen_function_call(scope, &function_call)
            }
            ast::Expression::Op(operation) => self.gen_op_expression(scope, &operation),
        }
    }

    pub fn gen_function_call(
        &mut self,
        scope: &Scope<'ctx>,
        function_call: &ast::FunctionCall,
    ) -> IntValue<'ctx> {
        let fn_value = self.module.get_function(&function_call.name.name).unwrap();
        let mut arguments = Vec::new();
        for expression in (&function_call.arguments).into_iter() {
            arguments.push(BasicValueEnum::IntValue(
                self.gen_expression(scope, &expression),
            ));
        }

        let result = self
            .builder
            .build_call(fn_value, &arguments, &function_call.name.name)
            .try_as_basic_value()
            .left()
            .unwrap();
        match result {
            BasicValueEnum::IntValue(value) => value,
            _ => panic!("function returned type other than int, no types yet"),
        }
    }

    // Generates a FunctionValue for an `ast::Function`. This does not genereate a body,
    // that task is left to the `gen_function` function. The reason this is split
    // between two functions is that first all signatures are generated and then all bodies. This
    // allows bodies to reference `FunctionValue` wherever they are declared in the file.
    pub fn gen_signature(&mut self, function: &ast::Function) -> FunctionValue {
        let mut args = Vec::new();
        for _ in &function.arguments {
            args.push(self.context.i64_type().into());
        }
        let fn_type = self.context.i64_type().fn_type(&args, false);
        let fn_value = self.module.add_function(&function.name.name, fn_type, None);
        fn_value
    }

    pub fn gen_function(&mut self, function: &ast::Function) {
        let fn_value = self.module.get_function(&function.name.name).unwrap();
        let basic_block = self.context.append_basic_block(fn_value, "entry");

        self.builder.position_at_end(basic_block);

        let mut scope = self.scope.clone();
        for (i, param) in (&function.arguments).into_iter().enumerate() {
            scope.insert(
                param.name.name.to_string(),
                fn_value.get_nth_param(i.try_into().unwrap()).unwrap(),
            );
        }
        let body = &function.block;
        let return_value = self.gen_expression(&scope, &body.expression);
        self.builder.build_return(Some(&return_value));
    }

    pub fn gen_module(&mut self, module: ast::Module) {
        // generate all signatures before the fuction bodies
        for function in &module.functions {
            self.gen_signature(&function);
        }
        for function in module.functions {
            self.gen_function(&function);
        }
    }

    pub fn dump(&self) -> String {
        self.module.print_to_string().to_string()
    }
}
