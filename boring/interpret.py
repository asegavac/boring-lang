import sys
import copy
from boring import parse
from dataclasses import dataclass


@dataclass
class Environment:
    identifiers: dict


class Interpreter:
    def handle_identifier(self, env, identifier):
        return env.identifiers[identifier.name]

    def handle_literal_int(self, env, literal_int):
        return literal_int.value

    def handle_function_call(self, env, function_call):
        new_env = copy.deepcopy(env)
        function_definition = new_env.identifiers[function_call.name.name]
        assert len(function_definition.arguments) == len(function_call.arguments)

        for i, argument in enumerate(function_definition.arguments):
            new_env.identifiers[argument.name.name] = self.handle_expression(
                env, function_call.arguments[i]
            )
        return self.handle_block(new_env, function_definition.block)

    def handle_operation(self, env, operation):
        if operation.op == parse.Operator.plus:
            return self.handle_expression(env, operation.left) + self.handle_expression(
                env, operation.right
            )
        elif operation.op == parse.Operator.minus:
            return self.handle_expression(env, operation.left) - self.handle_expression(
                env, operation.right
            )
        elif operation.op == parse.Operator.mult:
            return self.handle_expression(env, operation.left) * self.handle_expression(
                env, operation.right
            )
        elif operation.op == parse.Operator.div:
            return self.handle_expression(env, operation.left) / self.handle_expression(
                env, operation.right
            )

    def handle_expression(self, env, expression):
        if type(expression.expression) == parse.LiteralInt:
            return self.handle_literal_int(env, expression.expression)
        elif type(expression.expression) == parse.FunctionCall:
            return self.handle_function_call(env, expression.expression)
        elif type(expression.expression) == parse.Identifier:
            return self.handle_identifier(env, expression.expression)
        elif type(expression.expression) == parse.Operation:
            return self.handle_operation(env, expression.expression)
        elif type(expression.expression) == parse.Expression:
            return self.handle_expression(env, expression.expression)
        else:
            raise Exception(f"unexpected type: {type(expression.expression)}")

    def handle_block(self, env, block):
        return self.handle_expression(env, block.expression)

    def run(self, module):
        env = Environment(identifiers={})
        for function in module.functions:
            env.identifiers[function.name.name] = function

        if "main" not in env.identifiers:
            raise Exception("must have main function")

        return self.handle_function_call(
            env, parse.FunctionCall(name=parse.Identifier("main"), arguments=[])
        )


if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        tree = parse.boring_parser.parse(f.read())
        # print(tree)
        ast = parse.TreeToBoring().transform(tree)
        print(ast)
        result = Interpreter().run(ast)
        print(result)
        exit(result)
