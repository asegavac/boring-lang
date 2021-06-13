from dataclasses import dataclass
from typing import List, Dict, Optional, Union, Tuple


from boring import parse, typedefs


Identified = Union[
    parse.LetStatement, parse.Function, parse.VariableDeclaration, parse.TypeDeclaration
]
Environment = Dict[str, Identified]


@dataclass
class Context:
    environment: Environment
    current_function: Optional[parse.Function]

    def copy(self):
        return Context(self.environment.copy(), self.current_function)


def unify(ctx: Context, first, second) -> bool:
    changed: bool
    result, changed = type_compare(ctx, first.type, second.type)
    first.type = result
    second.type = result
    return changed


def type_compare(
    ctx: Context, first: parse.TypeUsage, second: parse.TypeUsage
) -> Tuple[parse.TypeUsage, bool]:
    print(first, second)
    if isinstance(first, parse.UnknownTypeUsage):
        if not isinstance(second, parse.UnknownTypeUsage):
            return second, True
        else:
            return parse.UnknownTypeUsage(), False
    else:
        if isinstance(second, parse.UnknownTypeUsage):
            return first, True
        else:
            if isinstance(first, parse.DataTypeUsage) and isinstance(
                second, parse.DataTypeUsage
            ):
                assert second == first
                assert first.name in ctx.environment  # TODO: validate that it is a type
                assert isinstance(
                    ctx.environment[first.name], parse.StructTypeDeclaration
                ) or isinstance(
                    ctx.environment[first.name], parse.PrimitiveTypeDeclaration
                )
                assert second.name in ctx.environment
                assert isinstance(
                    ctx.environment[second.name], parse.StructTypeDeclaration
                ) or isinstance(
                    ctx.environment[second.name], parse.PrimitiveTypeDeclaration
                )
                return first, False
            elif isinstance(first, parse.FunctionTypeUsage) and isinstance(
                second, parse.FunctionTypeUsage
            ):
                return_type, changed = type_compare(
                    ctx, first.return_type, second.return_type
                )
                arguments = []
                assert len(first.arguments) == len(second.arguments)
                for first_arg, second_arg in zip(first.arguments, second.arguments):
                    argument_type, argument_changed = type_compare(
                        ctx, first_arg, second_arg
                    )
                    arguments.append(argument_type)
                    if argument_changed:
                        changed = True
                return parse.FunctionTypeUsage(arguments, return_type), changed
            else:
                assert False, f"mismatched types {first}, {second}"


def assert_exists(ctx: Context, type: parse.TypeUsage):
    if isinstance(type, parse.DataTypeUsage):
        assert type.name in ctx.environment
    elif isinstance(type, parse.FunctionTypeUsage):
        assert_exists(ctx, type.return_type)
        for argument in type.arguments:
            assert_exists(ctx, argument)


class TypeChecker:
    def with_module(self, ctx: Context, module: parse.Module) -> bool:
        for type_declaration in module.types:
            ctx.environment[type_declaration.name] = type_declaration
        for type_declaration in module.types:
            if isinstance(type_declaration, parse.StructTypeDeclaration):
                for name, field in type_declaration.fields.items():
                    assert_exists(ctx, field)
        for function in module.functions:
            ctx.environment[function.name] = function

        changed = False
        for impl in module.impls:
            for function in impl.functions:
                if self.with_function(ctx, function):
                    changed = True

        for function in module.functions:
            if self.with_function(ctx, function):
                changed = True
        return changed

    def with_function(self, ctx: Context, function: parse.Function) -> bool:
        function_ctx = ctx.copy()
        function_ctx.current_function = function
        for argument in function.arguments:
            function_ctx.environment[argument.name] = argument
        assert isinstance(function.type, parse.FunctionTypeUsage)

        changed = self.with_block(function_ctx, function.block)

        if not (
            isinstance(function.block.type, parse.DataTypeUsage)
            and function.block.type.name == parse.NEVER_TYPE
        ):
            type, compare_changed = type_compare(
                function_ctx, function.block.type, function.type.return_type
            )
            function.block.type = type
            function.type.return_type = type
            if compare_changed is True:
                changed = True
        return changed

    # Skip variable VariableDeclaration

    def with_block(self, ctx: Context, block: parse.Block) -> bool:
        block_ctx = ctx.copy()
        # if parent is void, must be statement
        # if parent is type, must be expression
        changed = False
        for statement in block.statements:
            if self.with_statement(block_ctx, statement):
                changed = True
        final = block.statements[-1]
        if isinstance(final, parse.LetStatement):
            if isinstance(block.type, parse.UnknownTypeUsage):
                changed = True
                block.type = parse.DataTypeUsage(name=parse.UNIT_TYPE)
            else:
                assert block.type == parse.DataTypeUsage(name=parse.UNIT_TYPE)
        elif isinstance(final, parse.ReturnStatement):
            if isinstance(block.type, parse.UnknownTypeUsage):
                changed = True
                block.type = parse.DataTypeUsage(name=parse.NEVER_TYPE)
            else:
                assert block.type == parse.DataTypeUsage(name=parse.NEVER_TYPE)
        elif isinstance(final, parse.Expression):
            if unify(block_ctx, final, block):
                changed = True
        return changed

    def with_statement(self, ctx: Context, statement: parse.Statement) -> bool:
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

    def with_let_statement(
        self, ctx: Context, let_statement: parse.LetStatement
    ) -> bool:
        changed = False
        ctx.environment[let_statement.variable_name] = let_statement
        if self.with_expression(ctx, let_statement.expression):
            changed = True
        if unify(ctx, let_statement, let_statement.expression):
            changed = True
        return changed

    def with_assignment_statement(
        self, ctx: Context, assignment_statement: parse.AssignmentStatement
    ) -> bool:
        changed = False
        if self.with_expression(ctx, assignment_statement.expression):
            changed = True
        if isinstance(assignment_statement.source, parse.VariableUsage):
            self.with_variable_usage(ctx, assignment_statement.source)
        elif isinstance(assignment_statement.source, parse.StructGetter):
            self.with_struct_getter(ctx, assignment_statement.source)
        else:
            assert False

        if unify(
            ctx,
            assignment_statement,
            assignment_statement.source,
        ):
            changed = True
        if unify(ctx, assignment_statement, assignment_statement.expression):
            changed = True
        return changed

    def with_return_statement(
        self, ctx: Context, return_statement: parse.ReturnStatement
    ) -> bool:
        changed = self.with_expression(ctx, return_statement.source)

        # Doesn't match on an unreachable return
        if not (
            isinstance(return_statement.source.type, parse.DataTypeUsage)
            and return_statement.source.type.name == parse.NEVER_TYPE
        ):
            assert isinstance(ctx.current_function, parse.Function)
            assert isinstance(ctx.current_function.type, parse.FunctionTypeUsage)
            type, compare_changed = type_compare(
                ctx, return_statement.source.type, ctx.current_function.type.return_type
            )
            return_statement.source.type = type
            ctx.current_function.type.return_type = type
            if compare_changed is True:
                changed = True
        return changed

    def with_expression(self, ctx: Context, expression: parse.Expression) -> bool:
        subexpression = expression.expression
        changed = False

        if isinstance(subexpression, parse.LiteralInt):
            changed = self.with_literal_int(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        if isinstance(subexpression, parse.LiteralFloat):
            changed = self.with_literal_float(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        if isinstance(subexpression, parse.LiteralStruct):
            changed = self.with_literal_struct(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        if isinstance(subexpression, parse.FunctionCall):
            changed = self.with_function_call(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        if isinstance(subexpression, parse.StructGetter):
            changed = self.with_struct_getter(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        if isinstance(subexpression, parse.Block):
            changed = self.with_block(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        if isinstance(subexpression, parse.VariableUsage):
            changed = self.with_variable_usage(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        if isinstance(subexpression, parse.Operation):
            changed = self.with_operation(ctx, subexpression)
            if unify(ctx, subexpression, expression):
                changed = True
            return changed
        assert False

    def with_variable_usage(
        self, ctx: Context, variable_usage: parse.VariableUsage
    ) -> bool:
        return unify(ctx, variable_usage, ctx.environment[variable_usage.name])

    def with_operation(self, ctx: Context, operation: parse.Operation) -> bool:
        changed = False
        if self.with_expression(ctx, operation.left):
            changed = True
        if self.with_expression(ctx, operation.right):
            changed = True
        if unify(ctx, operation, operation.left):
            changed = True
        if unify(ctx, operation, operation.right):
            changed = True
        return changed

    def with_function_call(
        self, ctx: Context, function_call: parse.FunctionCall
    ) -> bool:
        changed = False
        if isinstance(function_call.source.type, parse.UnknownTypeUsage):
            function_call.source.type = parse.FunctionTypeUsage(
                arguments=[parse.UnknownTypeUsage()] * len(function_call.arguments),
                return_type=parse.UnknownTypeUsage(),
            )
            changed = True
        if self.with_expression(ctx, function_call.source):
            changed = True
        for argument in function_call.arguments:
            if self.with_expression(ctx, argument):
                changed = True

        assert isinstance(function_call.source.type, parse.FunctionTypeUsage)
        return_type, return_changed = type_compare(
            ctx, function_call.type, function_call.source.type.return_type
        )
        function_call.type = return_type
        function_call.source.type.return_type = return_type
        if return_changed:
            changed = True

        for argument, argument_type in zip(
            function_call.arguments, function_call.source.type.arguments
        ):
            argument_out_type, argument_changed = type_compare(
                ctx, argument.type, function_call.source.type.return_type
            )
            argument.type = argument_out_type
            function_call.source.type.return_type = argument_out_type
            if argument_changed:
                changed = True
        return changed

    def with_struct_getter(
        self, ctx: Context, struct_getter: parse.StructGetter
    ) -> bool:
        changed = self.with_expression(ctx, struct_getter.source)
        assert isinstance(struct_getter.source.type, parse.DataTypeUsage)
        struct_declaration = ctx.environment[struct_getter.source.type.name]
        assert isinstance(struct_declaration, parse.StructTypeDeclaration)
        assert struct_getter.attribute in struct_declaration.fields
        result_type, changed_getter = type_compare(
            ctx, struct_getter.type, struct_declaration.fields[struct_getter.attribute]
        )
        if changed_getter:
            changed = True
            struct_getter.type = result_type
        return changed

    def with_literal_float(
        self, ctx: Context, literal_float: parse.LiteralFloat
    ) -> bool:
        floats = ["F32", "F64", "F128"]
        if not isinstance(literal_float.type, parse.UnknownTypeUsage):
            assert isinstance(literal_float.type, parse.DataTypeUsage)
            assert literal_float.type.name in floats, f"{literal_float.type}"
        return False

    def with_literal_int(self, ctx: Context, literal_int: parse.LiteralInt) -> bool:
        ints = ["I8", "I16", "I32", "I64", "I128", "U8", "U16", "U32", "U64", "U128"]
        if not isinstance(literal_int.type, parse.UnknownTypeUsage):
            assert isinstance(literal_int.type, parse.DataTypeUsage)
            assert literal_int.type.name in ints, f"{literal_int.type}"
        return False

    def with_literal_struct(
        self, ctx: Context, literal_struct: parse.LiteralStruct
    ) -> bool:
        assert literal_struct.name in ctx.environment, literal_struct.name
        struct_declaration = ctx.environment[literal_struct.name]
        assert isinstance(struct_declaration, parse.StructTypeDeclaration)
        changed = False
        for name, field_type in struct_declaration.fields.items():
            assert name in literal_struct.fields
            if self.with_expression(ctx, literal_struct.fields[name]):
                changed = True
            result_type, field_changed = type_compare(
                ctx, field_type, literal_struct.fields[name].type
            )
            if field_changed:
                literal_struct.fields[name].type = result_type
                changed = True
        return changed
