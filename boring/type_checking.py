from dataclasses import dataclass
from typing import List, Dict, Optional, Union


from boring import parse


Identified = Union[parse.LetStatement, parse.Function, parse.VariableDeclaration]
Environment = Dict[str, Identified]


class TypeChecker:
    def with_module(self, env: Environment, module: parse.Module) -> bool:
        for function in module.functions:
            env[function.name.name] = function
        found = False
        for function in module.functions:
            if self.with_function(env, function):
                found = True
        return found

    def with_function(self, env: Environment, function: parse.Function) -> bool:
        function_env = env.copy()
        for argument in function.arguments:
            function_env[argument.name.name] = argument
        assert isinstance(function.type, parse.FunctionTypeUsage)
        function.block.type = function.type.return_type
        return self.with_block(function_env, function.block)

    # Skip variable VariableDeclaration

    def with_block(self, env: Environment, block: parse.Block) -> bool:
        block_env = env.copy()
        # if parent is void, must be statement
        # if parent is type, must be expression
        found = False
        final = block.statements[-1]
        if isinstance(final, parse.LetStatement):
            if block.type is None:
                found = True
                block.type = parse.DataTypeUsage(name=parse.Identifier(name=parse.UNIT_TYPE))
            else:
                assert block.type == parse.DataTypeUsage(name=parse.Identifier(name=parse.UNIT_TYPE))
        elif isinstance(final, parse.Expression):
            if block.type is None:
                if final.type is not None:
                    found = True
                    block.type = final.type
            else:
                if final.type is None:
                    found = True
                    final.type = block.type
                else:
                    assert final.type == block.type

        for statement in block.statements:
            if self.with_statement(block_env, statement):
                found = True
        return found

    def with_statement(self, env: Environment, statement: parse.Statement) -> bool:
        if isinstance(statement, parse.LetStatement):
            return self.with_let_statement(env, statement)
        elif isinstance(statement, parse.Expression):  # expression
            return self.with_expression(env, statement)
        else:
            assert False

    def with_let_statement(
        self, env: Environment, let_statement: parse.LetStatement
    ) -> bool:
        found = False
        env[let_statement.variable_name.name] = let_statement
        if let_statement.type is None:
            if let_statement.expression.type is not None:
                let_statement.type = let_statement.expression.type
                found = True
        else:
            if let_statement.expression.type is None:
                let_statement.expression.type = let_statement.type
                found = True
            else:
                assert let_statement.expression.type == let_statement.type
        if self.with_expression(env, let_statement.expression):
            found = True
        return found


    def with_expression(self, env: Environment, expression: parse.Expression) -> bool:
        subexpression = expression.expression
        found = False
        # generic to all types
        if expression.type is None:
            if subexpression.type is not None:
                expression.type = subexpression.type
                found = True
        else:
            if subexpression.type is None:
                subexpression.type = expression.type
                found = True
            else:
                assert subexpression.type == expression.type

        if isinstance(subexpression, parse.LiteralInt):
            if self.with_literal_int(env, subexpression):
                found = True
            return found
        if isinstance(subexpression, parse.FunctionCall):
            if self.with_function_call(env, subexpression):
                found = True
            return found
        if isinstance(subexpression, parse.VariableUsage):
            if self.with_variable_usage(env, subexpression):
                found = True
            return found
        if isinstance(subexpression, parse.Operation):
            if self.with_operation(env, subexpression):
                found = True
            return found
        assert False

    def with_variable_usage(
        self, env: Environment, variable_usage: parse.VariableUsage
    ) -> bool:
        found = False
        variable = env[variable_usage.name.name]
        if variable_usage.type is None:
            if variable.type is not None:
                variable_usage.type = variable.type
                found = True
        else:
            if variable.type is None:
                # print('@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@')
                # print(f"{variable.name} {variable.type}")
                variable.type = variable_usage.type
                found = True
            else:
                assert variable.type == variable_usage.type
        return found

    def with_operation(self, env: Environment, operation: parse.Operation) -> bool:
        found = False
        if operation.type is None:
            if operation.left.type is not None:
                operation.type = operation.left.type
                found = True
        else:
            if operation.left.type is None:
                operation.left.type = operation.type
                found = True
            else:
                assert operation.left.type == operation.type
        if operation.type is None:
            if operation.right.type is not None:
                operation.type = operation.right.type
                found = True
        else:
            if operation.right.type is None:
                operation.right.type = operation.type
                found = True
            else:
                assert operation.right.type == operation.type
        if self.with_expression(env, operation.left):
            found = True
        if self.with_expression(env, operation.right):
            found = True
        return found

    def with_function_call(
        self, env: Environment, function_call: parse.FunctionCall
    ) -> bool:
        found = False
        if function_call.type is None:
            if function_call.source.type is not None:
                assert isinstance(function_call.source.type, parse.FunctionTypeUsage)
                found = True
                function_call.type = function_call.source.type.return_type
        else:
            if function_call.source.type is not None:
                assert isinstance(function_call.source.type, parse.FunctionTypeUsage)
                assert function_call.type == function_call.source.type.return_type
        if self.with_expression(env, function_call.source):
            found = True

        if function_call.source.type is not None:
            assert isinstance(function_call.source.type, parse.FunctionTypeUsage)
            assert len(function_call.arguments) == len(function_call.source.type.arguments)
            for (argument, type_argument) in zip(function_call.arguments, function_call.source.type.arguments):
                if argument.type is None:
                    argument.type = type_argument
                    found = True
                else:
                    assert argument.type == type_argument

        for argument in function_call.arguments:
            if self.with_expression(env, argument):
                found = True
        return found


    def with_literal_int(self, env: Environment, literal_int: parse.LiteralInt) -> bool:
        ints = [parse.DataTypeUsage(name=parse.Identifier(name=name)) for name in ["Int8", "Int16", "Int32", "Int64", "Int128", "u32"]]
        if literal_int.type is not None:
            assert literal_int.type in ints, f"{literal_int.type}"
        return False
