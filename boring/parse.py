import sys
import enum
from typing import Union
from dataclasses import dataclass, field
from lark import Lark, Transformer


@dataclass
class Identifier:
    name: str


class Operator(enum.Enum):
    mult = "mult"
    div = "div"
    plus = "plus"
    minus = "minus"


@dataclass
class LiteralInt:
    value: int


@dataclass
class FunctionCall:
    name: Identifier
    arguments: list['Expression'] = field(default_factory=list)


@dataclass
class Operation:
    left: 'Expression'
    op: Operator
    right: 'Expression'


@dataclass
class Expression:
    expression: Union[LiteralInt,FunctionCall,Identifier,Operation,'Expression']


@dataclass
class Block:
    expression: Expression


@dataclass
class VariableDeclaration:
    name: Identifier


@dataclass
class Function:
    name: Identifier
    arguments: list[VariableDeclaration]
    block: Block


@dataclass
class Module:
    functions: Function



boring_grammar = r"""
    plus : "+"
    minus : "-"
    mult : "*"
    div : "/"

    literal_int: SIGNED_NUMBER
    identifier : NAME
    function_call : identifier "(" [expression ("," expression)*] ")"

    add_expression : expression plus factor
    sub_expression : expression minus factor
    mult_expression : expression mult term
    div_expression : expression div term

    expression : add_expression
               | sub_expression
               | factor

    factor : mult_expression
           | div_expression
           | term

    term : literal_int
         | identifier
         | function_call
         | "(" expression ")"

    block : "{" expression "}"

    variable_declaration : identifier

    function : "fn" identifier "(" [variable_declaration ("," variable_declaration)*] ")" block

    module : (function)*

    %import common.CNAME -> NAME
    %import common.SIGNED_NUMBER
    %import common.WS
    %ignore WS
    """

class TreeToBoring(Transformer):
    def plus(self, p):
        return Operator.plus

    def minus(self, m):
        return Operator.minus

    def mult(self, m):
        return Operator.mult

    def div(self, d):
        return Operator.div

    def literal_int(self, n):
        (n,) = n
        return LiteralInt(value=int(n))

    def identifier(self, i):
        (i,) = i
        return Identifier(name=str(i))

    def function_call(self, call):
        return FunctionCall(name=call[0], arguments=call[1:])

    def add_expression(self, ae):
        return Operation(left=ae[0], op=ae[1], right=ae[2])

    def sub_expression(self, se):
        return Operation(left=se[0], op=se[1], right=se[2])

    def mult_expression(self, se):
        return Operation(left=se[0], op=se[1], right=se[2])

    def div_expression(self, se):
        return Operation(left=se[0], op=se[1], right=se[2])

    def expression(self, exp):
        (exp,) = exp
        return Expression(expression=exp)

    def factor(self, factor):
        (factor,) = factor
        return Expression(factor)

    def term(self, term):
        (term,) = term
        return Expression(term)

    def block(self, block):
        (block,) = block
        return Block(expression=block)

    def variable_declaration(self, identifier):
        (identifier,) = identifier
        return VariableDeclaration(name=identifier)

    def function(self, function):
        return Function(name=function[0], arguments=function[1:-1], block=function[-1])

    def module(self, functions):
        return Module(functions=functions)


boring_parser = Lark(boring_grammar, start='module', lexer='standard')

if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        tree = boring_parser.parse(f.read())
        print(tree)
        print(TreeToBoring().transform(tree))
