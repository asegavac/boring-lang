import sys
import enum
from typing import Union, List
from dataclasses import dataclass, field
from lark import Lark, Transformer

def pretty_print(clas, indent=0):
    print(' ' * indent +  type(clas).__name__ +  ':')
    indent += 2
    for k,v in clas.__dict__.items():
        if '__dict__' in dir(v):
            print(' ' * indent +  k + ': ')
            pretty_print(v,indent+2)
        elif type(v) == list:
            print(' ' * indent + k + ': ' "[")
            for e in v:
                pretty_print(e, indent+2)
            print(' ' * indent + "]")
        else:
            print(' ' * indent +  k + ': ' + str(v))


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
    arguments: List['Expression'] = field(default_factory=list)


@dataclass
class Operation:
    left: 'Expression'
    op: Operator
    right: 'Expression'


@dataclass
class Expression:
    expression: Union[LiteralInt,FunctionCall,Identifier,Operation,'Expression']


@dataclass
class LetStatement:
    variable_name: Identifier
    expression: Expression


@dataclass
class Statement:
    statement: Union[LetStatement, Expression]


@dataclass
class Block:
    statements: List[Statement]


@dataclass
class VariableDeclaration:
    name: Identifier


@dataclass
class Function:
    name: Identifier
    arguments: List[VariableDeclaration]
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

    let_statement : "let" identifier "=" expression ";"

    statement : let_statement
              | expression

    block : "{" (statement)* "}"

    variable_declaration : identifier

    function : "fn" identifier "(" [variable_declaration ("," variable_declaration)*] ")" block

    module : (function)*

    %import common.CNAME -> NAME
    %import common.SIGNED_NUMBER
    %import common.WS
    %ignore WS
    """

class TreeToBoring(Transformer):
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
        return LiteralInt(value=int(n))

    def identifier(self, i) -> Identifier:
        (i,) = i
        return Identifier(name=str(i))

    def function_call(self, call) -> FunctionCall:
        return FunctionCall(name=call[0], arguments=call[1:])

    def add_expression(self, ae) -> Operation:
        return Operation(left=ae[0], op=ae[1], right=ae[2])

    def sub_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2])

    def mult_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2])

    def div_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2])

    def expression(self, exp) -> Expression:
        (exp,) = exp
        if type(exp) == Expression:
            return exp
        return Expression(expression=exp)

    def factor(self, factor) -> Expression:
        (factor,) = factor
        if type(factor) == Expression:
            return factor
        return Expression(factor)

    def term(self, term) -> Expression:
        (term,) = term
        return Expression(term)

    def let_statement(self, let_statement) -> LetStatement:
        (variable_name, expression) = let_statement
        return LetStatement(variable_name=variable_name, expression=expression)

    def statement(self, statement) -> Statement:
        (statement,) = statement
        return Statement(statement=statement)

    def block(self, block) -> Block:
        return Block(statements=block)

    def variable_declaration(self, identifier) -> VariableDeclaration:
        (identifier,) = identifier
        return VariableDeclaration(name=identifier)

    def function(self, function) -> Function:
        return Function(name=function[0], arguments=function[1:-1], block=function[-1])

    def module(self, functions) -> Module:
        return Module(functions=functions)


boring_parser = Lark(boring_grammar, start='module', lexer='standard')

if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        tree = boring_parser.parse(f.read())
        # print(tree)
        result = TreeToBoring().transform(tree)
        pretty_print(result)
