from dataclasses import dataclass
from typing import List, Dict, Optional, Union


from boring import parse


@dataclass
class EqualityTypeComparison:
    from_id: Optional[str]
    to_id: str
    type_usage: Optional[parse.TypeUsage]
    # constraints: List[Constraint]


@dataclass
class FunctionCallTypeComparison:
    from_id: str
    to_id: str
    type_usage: Optional[parse.TypeUsage]


@dataclass
class FunctionArgumentTypeComparison:
    from_id: str
    to_id: str
    argument_id: int
    type_usage: Optional[parse.TypeUsage]


TypeComparison = Union[
    EqualityTypeComparison, FunctionCallTypeComparison, FunctionArgumentTypeComparison
]
Environment = Dict[str, str]


class TypeCheckTableBuilder:
    def with_module(
        self, env: Environment, table: List[TypeComparison], module: parse.Module
    ):
        for function in module.functions:
            env[function.name.name] = function.id
            type_usage = parse.FunctionTypeUsage(
                arguments=[arg.type for arg in function.arguments],
                return_type=function.return_type,
            )
            table.append(
                EqualityTypeComparison(
                    from_id=None, to_id=function.id, type_usage=type_usage
                )
            )
        for function in module.functions:
            self.with_function(env, table, function)

    def with_function(
        self, env: Environment, table: List[TypeComparison], function: parse.Function
    ):
        function_env = env.copy()
        for argument in function.arguments:
            function_env[argument.name.name] = argument.id
            table.append(
                EqualityTypeComparison(
                    from_id=None, to_id=argument.id, type_usage=argument.type
                )
            )
        table.append(
            EqualityTypeComparison(
                from_id=None, to_id=function.block.id, type_usage=function.return_type
            )
        )
        self.with_block(function_env, table, function.block)

    # Skip variable VariableDeclaration

    def with_block(
        self, env: Environment, table: List[TypeComparison], block: parse.Block
    ):
        block_env = env.copy()
        # if parent is void, must be statement
        # if parent is type, must be expression
        if isinstance(block.statements[-1], parse.Expression):
            table.append(
                EqualityTypeComparison(
                    from_id=block.statements[-1].id, to_id=block.id, type_usage=None
                )
            )
        else:
            table.append(
                EqualityTypeComparison(
                    from_id=None,
                    to_id=block.id,
                    type_usage=parse.DataTypeUsage(
                        name=parse.Identifier(name=parse.UNIT_TYPE)
                    ),
                )
            )
        for statement in block.statements:
            print(statement)
            self.with_statement(block_env, table, statement)

    def with_statement(
        self, env: Environment, table: List[TypeComparison], statement: parse.Statement
    ):
        if isinstance(statement, parse.LetStatement):
            self.with_let_statement(env, table, statement)
        elif isinstance(statement, parse.Expression):  # expression
            self.with_expression(env, table, statement)
        else:
            assert False

    def with_let_statement(
        self,
        env: Environment,
        table: List[TypeComparison],
        let_statement: parse.LetStatement,
    ):
        env[let_statement.variable_name.name] = let_statement.id
        table.append(
            EqualityTypeComparison(
                from_id=let_statement.expression.id,
                to_id=let_statement.id,
                type_usage=let_statement.type,
            )
        )
        self.with_expression(env, table, let_statement.expression)

    def with_expression(
        self,
        env: Environment,
        table: List[TypeComparison],
        expression: parse.Expression,
    ):
        if isinstance(expression.expression, parse.LiteralInt):
            table.append(
                EqualityTypeComparison(
                    from_id=expression.expression.id,
                    to_id=expression.id,
                    type_usage=None,
                )
            )
            self.with_literal_int(env, table, expression.expression)
        elif isinstance(expression.expression, parse.FunctionCall):
            table.append(
                EqualityTypeComparison(
                    from_id=expression.expression.id,
                    to_id=expression.id,
                    type_usage=None,
                )
            )
            self.with_function_call(env, table, expression.expression)
        elif isinstance(expression.expression, parse.VariableUsage):
            table.append(
                EqualityTypeComparison(
                    from_id=expression.expression.id,
                    to_id=expression.id,
                    type_usage=None,
                )
            )
            self.with_variable_usage(env, table, expression.expression)
        elif isinstance(expression.expression, parse.Operation):
            table.append(
                EqualityTypeComparison(
                    from_id=expression.expression.id,
                    to_id=expression.id,
                    type_usage=None,
                )
            )
            self.with_operation(env, table, expression.expression)
        else:
            assert False

    def with_variable_usage(
        self,
        env: Environment,
        table: List[TypeComparison],
        variable_usage: parse.VariableUsage,
    ):
        print("%%%%%%%%%%%%%%%%%%%%%")
        print(env[variable_usage.name.name])
        print(variable_usage.id)
        table.append(
            EqualityTypeComparison(
                from_id=env[variable_usage.name.name],
                to_id=variable_usage.id,
                type_usage=None,
            )
        )

    def with_operation(
        self, env: Environment, table: List[TypeComparison], operation: parse.Operation
    ):
        table.append(
            EqualityTypeComparison(
                from_id=operation.left.id, to_id=operation.id, type_usage=None
            )
        )
        table.append(
            EqualityTypeComparison(
                from_id=operation.right.id, to_id=operation.id, type_usage=None
            )
        )
        self.with_expression(env, table, operation.left)
        self.with_expression(env, table, operation.right)

    def with_function_call(
        self,
        env: Environment,
        table: List[TypeComparison],
        function_call: parse.FunctionCall,
    ):
        table.append(
            EqualityTypeComparison(
                to_id=function_call.id, from_id=function_call.source.id, type_usage=None
            )
        )
        self.with_expression(env, table, function_call.source)

        for i, argument in enumerate(function_call.arguments):
            # table.append(
            #     FunctionArgumentTypeComparison(from_id=env[function_call.name.name], to_id=function_call.id, type_usage=None)
            # )
            # FunctionArgumentTypeComparison
            self.with_expression(env, table, argument)

    def with_literal_int(
        self,
        env: Environment,
        table: List[TypeComparison],
        literal_int: parse.LiteralInt,
    ):
        table.append(
            EqualityTypeComparison(
                from_id=None,
                to_id=literal_int.id,
                type_usage=parse.DataTypeUsage(
                    name=parse.Identifier(name="u32"),
                ),
            )
        )


def check_types(table: List[TypeComparison]):
    found = True
    while found:
        found = False
        for entry in table:
            for other in table:
                if other.to_id == entry.to_id:
                    if other.type_usage is None and entry.type_usage is None:
                        pass
                    elif other.type_usage is None and entry.type_usage is not None:
                        other.type_usage = entry.type_usage
                        found = True
                    elif other.type_usage is not None and entry.type_usage is None:
                        entry.type_usage = other.type_usage
                        found = True
                    else:
                        assert entry.type_usage == other.type_usage
                if other.from_id == entry.to_id:
                    # let a = || {4}     entry
                    # let b = a()        other
                    if other.type_usage is None and entry.type_usage is None:
                        pass
                    elif other.type_usage is None and entry.type_usage is not None:
                        if isinstance(other, EqualityTypeComparison):
                            other.type_usage = entry.type_usage
                            found = True
                        elif isinstance(other, FunctionCallTypeComparison):
                            assert isinstance(
                                entry.type_usage, parse.FunctionTypeUsage
                            ), "non function called"
                            other.type_usage = entry.type_usage.return_type
                            found = True
                    elif other.type_usage is not None and entry.type_usage is None:
                        if isinstance(other, EqualityTypeComparison):
                            entry.type_usage = other.type_usage
                            found = True
                        elif isinstance(other, FunctionCallTypeComparison):
                            pass  # can't reverse a function
                    else:
                        if isinstance(other, EqualityTypeComparison):
                            assert other.type_usage == entry.type_usage
                        elif isinstance(other, FunctionCallTypeComparison):
                            assert isinstance(
                                entry.type_usage, parse.FunctionTypeUsage
                            ), "non function called"
                            assert other.type_usage == entry.type_usage.return_type

                # if other.to_id == entry.from_id:
                #     # let a = || {4}     other
                #     # let b = a()        entry
                #     if other.type_usage is None and entry.type_usage is None:
                #         pass
                #     elif other.type_usage is None and entry.type_usage is not None:
                #         if isinstance(entry, EqualityTypeComparison):
                #             other.type_usage = entry.type_usage
                #             found = True
                #         elif isinstance(entry, FunctionCallTypeComparison):
                #             pass # can't reverse a function
                #     elif other.type_usage is not None and entry.type_usage is None:
                #         if isinstance(entry, EqualityTypeComparison):
                #             entry.type_usage = other.type_usage
                #             found = True
                #         elif isinstance(entry, FunctionCallTypeComparison):
                #             entry.type_usage = other.type_usage.return_type
                #             found = True
                # if other.from_id == entry.from_id and entry.from_id is not None:
                #     if other.type_usage is None and entry.type_usage is None:
                #         pass
                #     elif other.type_usage is None and entry.type_usage is not None:
                #         other.type_usage = entry.type_usage
                #         found = True
                #     elif other.type_usage is not None and entry.type_usage is None:
                #         entry.type_usage = other.type_usage
                #         found = True
                #     else:
                #         assert entry.type_usage == other.type_usage, f"{entry.from_id} {other.from_id} {entry.type_usage} == {other.type_usage}"
