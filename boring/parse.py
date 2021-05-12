import sys
import enum
from typing import Union, List, Optional
from dataclasses import dataclass, field
from lark import Lark, Transformer


def pretty_print(clas, indent=0):
    print(" " * indent + type(clas).__name__ + ":")
    if type(clas) == list:
        for e in clas:
            pretty_print(e)
        return
    indent += 2
    for k, v in clas.__dict__.items():
        if "__dict__" in dir(v):
            print(" " * indent + k + ": ")
            pretty_print(v, indent + 2)
        elif type(v) == list:
            print(" " * indent + k + ": " "[")
            for e in v:
                pretty_print(e, indent + 2)
            print(" " * indent + "]")
        else:
            print(" " * indent + k + ": " + str(v))


UNIT_TYPE = "()"


@dataclass
class Identifier:
    name: str


@dataclass
class FunctionTypeUsage:
    arguments: List[
        "TypeUsage"
    ]  # Specified if it is a function, this is how you tell if it's a function
    return_type: "TypeUsage"


@dataclass
class DataTypeUsage:
    name: Identifier


TypeUsage = Union[FunctionTypeUsage, DataTypeUsage]


class Operator(enum.Enum):
    mult = "mult"
    div = "div"
    plus = "plus"
    minus = "minus"


@dataclass
class LiteralInt:
    value: int
    type: Optional[TypeUsage]


@dataclass
class FunctionCall:
    source: "Expression"
    arguments: List["Expression"]
    type: Optional[TypeUsage]


@dataclass
class Operation:
    left: "Expression"
    op: Operator
    right: "Expression"
    type: Optional[TypeUsage]


@dataclass
class VariableUsage:
    name: Identifier
    type: Optional[TypeUsage]


@dataclass
class Expression:
    expression: Union[LiteralInt, FunctionCall, VariableUsage, Operation]
    type: Optional[TypeUsage]


@dataclass
class LetStatement:
    variable_name: Identifier
    type: Optional[TypeUsage]
    expression: Expression


Statement = Union[LetStatement, Expression]


@dataclass
class Block:
    statements: List[Statement]
    type: Optional[TypeUsage]


@dataclass
class VariableDeclaration:
    name: Identifier
    type: TypeUsage


@dataclass
class Function:
    name: Identifier
    arguments: List[VariableDeclaration]
    block: Block
    return_type: TypeUsage
    type: TypeUsage


@dataclass
class Module:
    functions: List[Function]


boring_grammar = r"""
    plus : "+"
    minus : "-"
    mult : "*"
    div : "/"

    literal_int: SIGNED_NUMBER
    identifier : NAME

    function_call : expression "(" [expression ("," expression)*] ")"

    add_expression : expression plus factor
    sub_expression : expression minus factor
    mult_expression : expression mult term
    div_expression : expression div term

    variable_usage : identifier

    expression : add_expression
               | sub_expression
               | factor

    factor : mult_expression
           | div_expression
           | term

    term : literal_int
         | variable_usage
         | function_call
         | "(" expression ")"

    let_statement : "let" identifier "=" expression ";"
                  | "let" identifier ":" type_usage "=" expression ";"

    statement : let_statement
              | expression

    block : "{" (statement)* "}"

    data_type : identifier

    function_type : "fn" "(" (type_usage)* ")"

    function_type_with_return : "fn" "(" (type_usage)* ")" ":" type_usage

    type_usage : data_type
               | function_type
               | function_type_with_return

    variable_declaration : identifier ":" type_usage

    function_without_return : "fn" identifier "(" [variable_declaration ("," variable_declaration)*] ")" block

    function_with_return : "fn" identifier "(" [variable_declaration ("," variable_declaration)*] ")" ":" type_usage block

    function : function_with_return
             | function_without_return

    module : (function)*

    %import common.CNAME -> NAME
    %import common.SIGNED_NUMBER
    %import common.WS
    %ignore WS
    """


class TreeToBoring(Transformer):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def plus(self, p) -> Operator:
        return Operator.plus

    def minus(self, m) -> Operator:
        return Operator.minus

    def mult(self, m) -> Operator:
        return Operator.mult

    def div(self, d) -> Operator:
        return Operator.div

    def literal_int(self, n) -> LiteralInt:
        (n,) = n
        return LiteralInt(value=int(n), type=None)

    def identifier(self, i) -> Identifier:
        (i,) = i
        return Identifier(name=str(i))

    def variable_usage(self, variable) -> VariableUsage:
        (variable,) = variable
        return VariableUsage(name=variable, type=None)

    def function_call(self, call) -> FunctionCall:
        return FunctionCall(source=call[0], arguments=call[1:], type=None)

    def add_expression(self, ae) -> Operation:
        return Operation(left=ae[0], op=ae[1], right=ae[2], type=None)

    def sub_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2], type=None)

    def mult_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2], type=None)

    def div_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2], type=None)

    def expression(self, exp) -> Expression:
        (exp,) = exp
        if isinstance(exp, Expression):
            return exp
        return Expression(expression=exp, type=None)

    def factor(self, factor) -> Expression:
        (factor,) = factor
        if isinstance(factor, Expression):
            return factor
        return Expression(expression=factor, type=None)

    def term(self, term) -> Expression:
        (term,) = term
        return Expression(expression=term, type=None)

    def let_statement(self, let_statement) -> LetStatement:
        if len(let_statement) == 3:
            (variable_name, type_usage, expression) = let_statement
            return LetStatement(
                variable_name=variable_name,
                type=type_usage,
                expression=expression,
            )
        (variable_name, expression) = let_statement
        return LetStatement(
            variable_name=variable_name,
            type=None,
            expression=expression,
        )

    def statement(self, statement):
        (statement,) = statement
        return statement

    def block(self, block) -> Block:
        return Block(statements=block, type=None)

    def data_type(self, name) -> TypeUsage:
        (name,) = name
        return DataTypeUsage(name=name)

    def function_type(self, type_usage) -> TypeUsage:
        return FunctionTypeUsage(
            arguments=type_usage,
            return_type=DataTypeUsage(name=Identifier(name=UNIT_TYPE)),
        )

    def function_type_with_return(self, type_usage) -> TypeUsage:
        return FunctionTypeUsage(arguments=type_usage[0:-1], return_type=type_usage[-1])

    def type_usage(self, type_usage):
        (type_usage,) = type_usage
        return type_usage

    def variable_declaration(self, identifier) -> VariableDeclaration:
        (identifier, type_usage) = identifier
        return VariableDeclaration(name=identifier, type=type_usage)

    def function_without_return(self, function) -> Function:
        return Function(
            name=function[0],
            arguments=function[1:-1],
            return_type=DataTypeUsage(name=Identifier(name=UNIT_TYPE)),
            block=function[-1],
            type=FunctionTypeUsage(
                arguments=[arg.type for arg in function[1:-1]],
                return_type=DataTypeUsage(name=Identifier(name=UNIT_TYPE)),
            ),
        )

    def function_with_return(self, function) -> Function:
        return Function(
            name=function[0],
            arguments=function[1:-2],
            return_type=function[-2],
            block=function[-1],
            type=FunctionTypeUsage(
                arguments=[arg.type for arg in function[1:-2]], return_type=function[-2]
            ),
        )

    def function(self, function):
        (function,) = function
        return function

    def module(self, functions) -> Module:
        return Module(functions=functions)


boring_parser = Lark(boring_grammar, start="module", lexer="standard")

if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        tree = boring_parser.parse(f.read())
        # print(tree)
        result = TreeToBoring().transform(tree)
        pretty_print(result)
