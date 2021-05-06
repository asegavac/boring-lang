from dataclasses import dataclass
from typing import List, Dict, Optional, Union


from boring import parse


Identified = Union[parse.LetStatement, parse.Function]
Environment = Dict[str, Identified]


class TypeChecker:
    def with_module(
        self, env: Environment, table: List[TypeComparison], module: parse.Module
    ) -> bool:
        pass

    def with_function(
        self, env: Environment, table: List[TypeComparison], function: parse.Function
    ) -> bool:
        pass

    # Skip variable VariableDeclaration

    def with_block(
        self, env: Environment, table: List[TypeComparison], block: parse.Block
    ) -> bool:
        pass

    def with_statement(
        self, env: Environment, table: List[TypeComparison], statement: parse.Statement
    ) -> bool:
        pass

    def with_let_statement(
        self, env: Environment, table: List[TypeComparison], let_statement: parse.LetStatement
    ) -> bool:
        pass

    def with_expression(
        self, env: Environment, table: List[TypeComparison], expression: parse.Expression
    ) -> bool:
        pass

    def with_variable_usage(
        self, env: Environment, table: List[TypeComparison], variable_usage: parse.VariableUsage
    ) -> bool:
        pass

    def with_operation(
        self, env: Environment, table: List[TypeComparison], operation: parse.Operation
    ) -> bool:
        pass

    def with_function_call(
        self, env: Environment, table: List[TypeComparison], function_call: parse.FunctionCall
    ) -> bool:
        pass


    def with_literal_int(
        self, env: Environment, table: List[TypeComparison], literal_int: parse.LiteralInt
    ) -> bool:
        pass
