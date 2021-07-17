from dataclasses import dataclass
from typing import List, Dict, Optional, Union, Tuple
from boring import parse


@dataclass
class Context:
    type_aliases: List[parse.AliasTypeDeclaration]

    def copy(self):
        return Context(self.type_aliases.copy())


def resolve_type(ctx: Context, type: parse.DataTypeUsage) -> parse.TypeUsage:
    changed = True
    result: parse.TypeUsage = type
    while changed:
        changed = False
        if isinstance(result, parse.DataTypeUsage):
            for type_alias in ctx.type_aliases:
                if type_alias.new.name == result.name:  # type: ignore
                    result = type_alias.old
                    changed = True
        else:
            break
    return result


def process_type(ctx: Context, type: parse.TypeUsage) -> parse.TypeUsage:
    if isinstance(type, parse.DataTypeUsage):
        return resolve_type(ctx, type)
    elif isinstance(type, parse.FunctionTypeUsage):
        return parse.FunctionTypeUsage(
            return_type=process_type(ctx, type.return_type),
            arguments=[process_type(ctx, argument) for argument in type.arguments],
        )
    else:
        return type


class TypeAliasResolver:
    def with_module(self, ctx: Context, module: parse.Module):
        for type_declaration in module.types:
            if isinstance(type_declaration, parse.AliasTypeDeclaration):
                ctx.type_aliases.append(type_declaration)

        for type_declaration in module.types:
            if isinstance(type_declaration, parse.StructTypeDeclaration):
                for field in type_declaration.fields:
                    type_declaration.fields[field] = process_type(
                        ctx, type_declaration.fields[field]
                    )

        for impl in module.impls:
            impl_ctx = ctx.copy()
            impl_ctx.type_aliases.append(
                parse.AliasTypeDeclaration(
                    new=parse.DataTypeUsage("Self"),
                    old=parse.DataTypeUsage(impl.struct),
                )
            )
            for function in impl.functions:
                self.with_function(impl_ctx, function)

        for function in module.functions:
            self.with_function(ctx, function)
        return

    def with_function(self, ctx: Context, function: parse.Function):
        for argument in function.declaration.arguments:
            argument.type = process_type(ctx, argument.type)
        function.declaration.return_type = process_type(ctx, function.declaration.return_type)
        function.declaration.type = process_type(ctx, function.declaration.type)

        self.with_block(ctx, function.block)
        return

    # Skip variable VariableDeclaration

    def with_block(self, ctx: Context, block: parse.Block):
        for statement in block.statements:
            self.with_statement(ctx, statement)
        block.type = process_type(ctx, block.type)

    def with_statement(self, ctx: Context, statement: parse.Statement):
        if isinstance(statement, parse.ReturnStatement):
            return self.with_return_statement(ctx, statement)
        elif isinstance(statement, parse.LetStatement):
            return self.with_let_statement(ctx, statement)
        elif isinstance(statement, parse.AssignmentStatement):
            return self.with_assignment_statement(ctx, statement)
        elif isinstance(statement, parse.Expression):  # expression
            return self.with_expression(ctx, statement)
        else:
            assert False

    def with_let_statement(self, ctx: Context, let_statement: parse.LetStatement):
        self.with_expression(ctx, let_statement.expression)
        let_statement.type = process_type(ctx, let_statement.type)

    def with_assignment_statement(
        self, ctx: Context, assignment_statement: parse.AssignmentStatement
    ):
        self.with_expression(ctx, assignment_statement.expression)

        if isinstance(assignment_statement.source, parse.VariableUsage):
            self.with_variable_usage(ctx, assignment_statement.source)
        elif isinstance(assignment_statement.source, parse.StructGetter):
            self.with_struct_getter(ctx, assignment_statement.source)
        else:
            assert False

        assignment_statement.type = process_type(ctx, assignment_statement.type)
        return

    def with_return_statement(
        self, ctx: Context, return_statement: parse.ReturnStatement
    ):
        self.with_expression(ctx, return_statement.source)
        return_statement.type = process_type(ctx, return_statement.type)
        return

    def with_expression(self, ctx: Context, expression: parse.Expression):
        subexpression = expression.expression
        expression.type = process_type(ctx, expression.type)

        if isinstance(subexpression, parse.LiteralInt):
            self.with_literal_int(ctx, subexpression)
        elif isinstance(subexpression, parse.LiteralFloat):
            self.with_literal_float(ctx, subexpression)
        elif isinstance(subexpression, parse.LiteralStruct):
            self.with_literal_struct(ctx, subexpression)
        elif isinstance(subexpression, parse.FunctionCall):
            self.with_function_call(ctx, subexpression)
        elif isinstance(subexpression, parse.StructGetter):
            self.with_struct_getter(ctx, subexpression)
        elif isinstance(subexpression, parse.Block):
            self.with_block(ctx, subexpression)
        elif isinstance(subexpression, parse.VariableUsage):
            self.with_variable_usage(ctx, subexpression)
        elif isinstance(subexpression, parse.Operation):
            self.with_operation(ctx, subexpression)
        else:
            assert False
        return

    def with_variable_usage(self, ctx: Context, variable_usage: parse.VariableUsage):
        variable_usage.type = process_type(ctx, variable_usage.type)

    def with_operation(self, ctx: Context, operation: parse.Operation):
        self.with_expression(ctx, operation.left)
        self.with_expression(ctx, operation.right)
        operation.type = process_type(ctx, operation.type)
        return

    def with_function_call(self, ctx: Context, function_call: parse.FunctionCall):
        self.with_expression(ctx, function_call.source)
        for argument in function_call.arguments:
            self.with_expression(ctx, argument)
        function_call.type = process_type(ctx, function_call.type)
        return

    def with_struct_getter(self, ctx: Context, struct_getter: parse.StructGetter):
        self.with_expression(ctx, struct_getter.source)
        struct_getter.type = process_type(ctx, struct_getter.type)
        return

    def with_literal_float(self, ctx: Context, literal_float: parse.LiteralFloat):
        literal_float.type = process_type(ctx, literal_float.type)
        return

    def with_literal_int(self, ctx: Context, literal_int: parse.LiteralInt):
        literal_int.type = process_type(ctx, literal_int.type)
        return

    def with_literal_struct(self, ctx: Context, literal_struct: parse.LiteralStruct):
        for name, expression in literal_struct.fields.items():
            self.with_expression(ctx, expression)
        literal_struct.type = process_type(ctx, literal_struct.type)
