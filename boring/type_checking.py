from dataclasses import dataclass
from typing import List, Dict, Optional, Union


from boring import parse, typedefs


Identified = Union[parse.LetStatement, parse.Function, parse.VariableDeclaration]
Environment = Dict[str, Identified]
TypeEnvironment = Dict[str, typedefs.TypeDef]


def unify(type_env: TypeEnvironment, first, second) -> bool:
    result, changed = type_compare(type_env, first.type, second.type)
    first.type = result
    second.type = result
    return changed


def type_compare(type_env: TypeEnvironment, first, second) -> (parse.TypeUsage, bool):
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
                assert first.name in type_env
                assert second.name in type_env
                return first, False
            elif isinstance(first, parse.FunctionTypeUsage) and isinstance(
                second, parse.FunctionTypeUsage
            ):
                return_type, changed = type_compare(
                    type_env, first.return_type, second.return_type
                )
                arguments = []
                assert len(first.arguments) == len(second.arguments)
                for first_arg, second_arg in zip(first.arguments, second.arguments):
                    argument_type, argument_changed = type_compare(
                        type_env, first_arg, second_arg
                    )
                    arguments.append(argument_type)
                    if argument_changed:
                        changed = True
                return parse.FunctionTypeUsage(arguments, return_type), changed
            else:
                assert False, f"mismatched types {first}, {second}"


class TypeChecker:
    def with_module(self, env: Environment, type_env: TypeEnvironment, module: parse.Module) -> bool:
        for function in module.functions:
            env[function.name] = function
        found = False
        for function in module.functions:
            if self.with_function(env, type_env, function):
                found = True
        return found

    def with_function(self, env: Environment, type_env: TypeEnvironment, function: parse.Function) -> bool:
        function_env = env.copy()
        for argument in function.arguments:
            function_env[argument.name] = argument
        assert isinstance(function.type, parse.FunctionTypeUsage)

        type, changed = type_compare(type_env, function.block.type, function.type.return_type)
        function.block.type = type
        function.type.return_type = type
        if self.with_block(function_env, type_env, function.block):
            changed = True
        return changed

    # Skip variable VariableDeclaration

    def with_block(self, env: Environment, type_env: TypeEnvironment, block: parse.Block) -> bool:
        block_env = env.copy()
        # if parent is void, must be statement
        # if parent is type, must be expression
        changed = False
        final = block.statements[-1]
        if isinstance(final, parse.LetStatement):
            if isinstance(block.type, parse.UnknownTypeUsage):
                found = True
                block.type = parse.DataTypeUsage(
                    name=parse.Identifier(name=parse.UNIT_TYPE)
                )
            else:
                assert block.type == parse.DataTypeUsage(
                    name=parse.Identifier(name=parse.UNIT_TYPE)
                )
        elif isinstance(final, parse.Expression):
            if unify(type_env, final, block):
                changed = True

        for statement in block.statements:
            if self.with_statement(block_env, type_env, statement):
                changed = True
        return changed

    def with_statement(self, env: Environment, type_env: TypeEnvironment, statement: parse.Statement) -> bool:
        if isinstance(statement, parse.LetStatement):
            return self.with_let_statement(env, type_env, statement)
        elif isinstance(statement, parse.Expression):  # expression
            return self.with_expression(env, type_env, statement)
        else:
            assert False

    def with_let_statement(
        self, env: Environment, type_env: TypeEnvironment, let_statement: parse.LetStatement
    ) -> bool:
        found = False
        env[let_statement.variable_name] = let_statement
        changed = unify(type_env, let_statement, let_statement.expression)
        if self.with_expression(env, type_env, let_statement.expression):
            changed = True
        return changed

    def with_expression(self, env: Environment, type_env: TypeEnvironment, expression: parse.Expression) -> bool:
        subexpression = expression.expression
        changed = unify(type_env, subexpression, expression)

        if isinstance(subexpression, parse.LiteralInt):
            if self.with_literal_int(env, type_env, subexpression):
                changed = True
            return changed
        if isinstance(subexpression, parse.LiteralFloat):
            if self.with_literal_float(env, type_env, subexpression):
                changed = True
            return changed
        if isinstance(subexpression, parse.FunctionCall):
            if self.with_function_call(env, type_env, subexpression):
                changed = True
            return changed
        if isinstance(subexpression, parse.VariableUsage):
            if self.with_variable_usage(env, type_env, subexpression):
                changed = True
            return changed
        if isinstance(subexpression, parse.Operation):
            if self.with_operation(env, type_env, subexpression):
                changed = True
            return changed
        assert False

    def with_variable_usage(
        self, env: Environment, type_env: TypeEnvironment, variable_usage: parse.VariableUsage
    ) -> bool:
        return unify(type_env, variable_usage, env[variable_usage.name])

    def with_operation(self, env: Environment, type_env: TypeEnvironment, operation: parse.Operation) -> bool:
        changed = False
        if unify(type_env, operation, operation.left):
            changed = True
        if unify(type_env, operation, operation.right):
            changed = True
        if self.with_expression(env, type_env, operation.left):
            changed = True
        if self.with_expression(env, type_env, operation.right):
            changed = True
        return changed

    def with_function_call(
        self, env: Environment, type_env: TypeEnvironment, function_call: parse.FunctionCall
    ) -> bool:
        changed = False
        if isinstance(function_call.source.type, parse.UnknownTypeUsage):
            function_call.source.type = parse.FunctionTypeUsage(
                arguments=[parse.UnknownTypeUsage()] * len(function_call.arguments),
                return_type=parse.UnknownTypeUsage(),
            )
            changed = True
        if self.with_expression(env, type_env, function_call.source):
            changed = True
        for argument in function_call.arguments:
            if self.with_expression(env, type_env, argument):
                changed = True

        return_type, return_changed = type_compare(
            type_env, function_call.type, function_call.source.type.return_type
        )
        function_call.type = return_type
        function_call.source.type.return_type = return_type
        if return_changed:
            changed = True

        for argument, argument_type in zip(
            function_call.arguments, function_call.source.type.arguments
        ):
            argument_out_type, argument_changed = type_compare(
                type_env, argument.type, function_call.source.type.return_type
            )
            argument.type = argument_out_type
            function_call.source.type.return_type = argument_out_type
            if argument_changed:
                changed = True
        return changed

    def with_literal_float(self, env: Environment, type_env: TypeEnvironment, literal_float: parse.LiteralFloat) -> bool:
        floats = ["F32", "F64", "F128"]
        if not isinstance(literal_float.type, parse.UnknownTypeUsage):
            assert literal_float.type.name in floats, f"{literal_float.type}"
        return False

    def with_literal_int(self, env: Environment, type_env: TypeEnvironment, literal_int: parse.LiteralInt) -> bool:
        ints = ["I8", "I16", "I32", "I64", "I128", "U8", "U16", "U32", "U64", "U128"]
        if not isinstance(literal_int.type, parse.UnknownTypeUsage):
            assert literal_int.type.name in ints, f"{literal_int.type}"
        return False
