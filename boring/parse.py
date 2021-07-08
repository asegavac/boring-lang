import sys
import enum
from typing import Union, List, Optional, Dict
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
NEVER_TYPE = "!"


@dataclass
class FunctionTypeUsage:
    arguments: List["TypeUsage"]
    return_type: "TypeUsage"


@dataclass
class DataTypeUsage:
    name: str


@dataclass
class UnknownTypeUsage:
    pass


TypeUsage = Union[FunctionTypeUsage, DataTypeUsage, UnknownTypeUsage]


class Operator(enum.Enum):
    mult = "mult"
    div = "div"
    plus = "plus"
    minus = "minus"


@dataclass
class LiteralInt:
    value: int
    type: TypeUsage


@dataclass
class LiteralFloat:
    value: float
    type: TypeUsage


@dataclass
class LiteralStruct:
    fields: Dict[str, "Expression"]
    type: TypeUsage


@dataclass
class FunctionCall:
    source: "Expression"
    arguments: List["Expression"]
    type: TypeUsage


@dataclass
class StructGetter:
    source: "Expression"
    attribute: str
    type: TypeUsage


@dataclass
class Operation:
    left: "Expression"
    op: Operator
    right: "Expression"
    type: TypeUsage


@dataclass
class VariableUsage:
    name: str
    type: TypeUsage


@dataclass
class ReturnStatement:
    source: "Expression"
    type: TypeUsage


@dataclass
class Expression:
    expression: Union[
        LiteralInt,
        LiteralFloat,
        LiteralStruct,
        FunctionCall,
        StructGetter,
        "Block",
        ReturnStatement,
        VariableUsage,
        Operation,
    ]
    type: TypeUsage


@dataclass
class LetStatement:
    variable_name: str
    type: TypeUsage
    expression: Expression


@dataclass
class AssignmentStatement:
    source: Union[VariableUsage, StructGetter]
    type: TypeUsage
    expression: Expression


Statement = Union[LetStatement, AssignmentStatement, Expression]


@dataclass
class Block:
    statements: List[Statement]
    type: TypeUsage


@dataclass
class VariableDeclaration:
    name: str
    type: TypeUsage


@dataclass
class Function:
    declaration: "FunctionDeclaration"
    block: Block
    type: TypeUsage


@dataclass
class FunctionDeclaration:
    name: str
    arguments: List[VariableDeclaration]
    return_type: TypeUsage
    type: TypeUsage


@dataclass
class PrimitiveTypeDeclaration:
    name: str


@dataclass
class StructTypeDeclaration:
    name: str
    fields: Dict[str, TypeUsage]


@dataclass
class AliasTypeDeclaration:
    new: DataTypeUsage
    old: TypeUsage


TypeDeclaration = Union[
    StructTypeDeclaration, PrimitiveTypeDeclaration, AliasTypeDeclaration
]


@dataclass
class Impl:
    struct: str
    functions: List[Function]


@dataclass
class FunctionDeclartation:
    name: str
    arguments: List[VariableDeclaration]
    return_type: TypeUsage
    type: TypeUsage


TraitItem = Union[FunctionDeclaration, Function]


@dataclass
class TraitTypeDeclaration:
    struct: str
    items: List[TraitItem]


@dataclass
class TraitImpl:
    struct: str
    trait: str
    functions: List[Function]


@dataclass
class Module:
    functions: List[Function]
    types: List[TypeDeclaration]
    impls: List[Impl]


boring_grammar = r"""
    plus : "+"
    minus : "-"
    mult : "*"
    div : "/"

    identifier : CNAME
    literal_float : SIGNED_FLOAT
    literal_int : SIGNED_INT

    literal_struct_field : identifier ":" expression
    literal_struct : data_type "{" (literal_struct_field ",")* "}"

    function_call : expression "(" [expression ("," expression)*] ")"

    struct_getter : expression "." identifier

    add_expression : expression plus factor
    sub_expression : expression minus factor
    mult_expression : expression mult term
    div_expression : expression div term

    variable_usage : identifier

    return_statement : "return" expression ";"

    expression : add_expression
               | sub_expression
               | factor

    factor : mult_expression
           | div_expression
           | term

    term : literal_int
         | literal_float
         | literal_struct
         | variable_usage
         | function_call
         | struct_getter
         | "(" expression ")"
         | block

    let_statement : "let" identifier "=" expression ";"
                  | "let" identifier ":" type_usage "=" expression ";"

    assignment_statement : variable_usage "=" expression ";"
                         | struct_getter "=" expression ";"

    statement : let_statement
              | assignment_statement
              | return_statement
              | expression

    block : "{" (statement)* "}"

    data_type : identifier

    function_type : "fn" "(" (type_usage)* ")"

    function_type_with_return : "fn" "(" (type_usage)* ")" ":" type_usage

    type_usage : data_type
               | function_type
               | function_type_with_return

    variable_declaration : identifier ":" type_usage

    function_declaration_without_return : "fn" identifier "(" [variable_declaration ("," variable_declaration)*] ")"

    function_declaration_with_return : "fn" identifier "(" [variable_declaration ("," variable_declaration)*] ")" ":" type_usage

    function_declaration : function_declaration_with_return
                         | function_declaration_without_return

    function : function_declaration block

    struct_definition_field : identifier ":" type_usage

    struct_type_declaration : "type" identifier "struct" "{" (struct_definition_field ",")* "}"

    type_alias_declaration : "type" identifier "=" type_usage ";"


    trait_item : function_declaration ";"
               | function

    trait_declaration : "type" identifier "trait" "{" trait_item* "}"

    type_declaration : struct_type_declaration
                     | type_alias_declaration

    impl : "impl" identifier "{" function* "}"
         | "impl" identifier "for" identifier "{" function* "}"

    module : (function|type_declaration|impl)*

    %import common.CNAME
    %import common.SIGNED_INT
    %import common.SIGNED_FLOAT
    %import common.WS
    %import common.CPP_COMMENT
    %ignore WS
    %ignore CPP_COMMENT
    """

next_sub_id = 0


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
        return LiteralInt(value=int(n), type=UnknownTypeUsage())

    def literal_float(self, f) -> LiteralFloat:
        (f,) = f
        return LiteralFloat(value=float(f), type=UnknownTypeUsage())

    def literal_struct_field(self, lsf):
        (name, expression) = lsf
        return name, expression

    def literal_struct(self, literal_struct) -> LiteralStruct:
        data_type = literal_struct[0]
        fields = {key: value for (key, value) in literal_struct[1:]}
        return LiteralStruct(fields=fields, type=data_type)

    def identifier(self, i) -> str:
        (i,) = i
        return str(i)

    def variable_usage(self, variable) -> VariableUsage:
        (variable,) = variable
        return VariableUsage(name=variable, type=UnknownTypeUsage())

    def return_statement(self, return_expression) -> ReturnStatement:
        (return_expression,) = return_expression
        return ReturnStatement(
            source=return_expression, type=DataTypeUsage(name=NEVER_TYPE)
        )

    def function_call(self, call) -> FunctionCall:
        return FunctionCall(source=call[0], arguments=call[1:], type=UnknownTypeUsage())

    def struct_getter(self, getter) -> StructGetter:
        expression, attribute = getter
        return StructGetter(expression, attribute, UnknownTypeUsage())

    def add_expression(self, ae) -> Operation:
        return Operation(left=ae[0], op=ae[1], right=ae[2], type=UnknownTypeUsage())

    def sub_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2], type=UnknownTypeUsage())

    def mult_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2], type=UnknownTypeUsage())

    def div_expression(self, se) -> Operation:
        return Operation(left=se[0], op=se[1], right=se[2], type=UnknownTypeUsage())

    def expression(self, exp) -> Expression:
        (exp,) = exp
        if isinstance(exp, Expression):
            return exp
        return Expression(expression=exp, type=UnknownTypeUsage())

    def factor(self, factor) -> Expression:
        (factor,) = factor
        if isinstance(factor, Expression):
            return factor
        return Expression(expression=factor, type=UnknownTypeUsage())

    def term(self, term) -> Expression:
        (term,) = term
        return Expression(expression=term, type=UnknownTypeUsage())

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
            type=UnknownTypeUsage(),
            expression=expression,
        )

    def assignment_statement(self, assignment_statement) -> AssignmentStatement:
        (source, expression) = assignment_statement
        return AssignmentStatement(
            source=source,
            type=UnknownTypeUsage(),
            expression=expression,
        )

    def statement(self, statement):
        (statement,) = statement
        return statement

    def block(self, block) -> Block:
        return Block(statements=block, type=UnknownTypeUsage())

    def data_type(self, name) -> TypeUsage:
        (name,) = name
        return DataTypeUsage(name=name)

    def function_type(self, type_usage) -> TypeUsage:
        return FunctionTypeUsage(
            arguments=type_usage,
            return_type=DataTypeUsage(name=UNIT_TYPE),
        )

    def function_type_with_return(self, type_usage) -> TypeUsage:
        return FunctionTypeUsage(arguments=type_usage[0:-1], return_type=type_usage[-1])

    def type_usage(self, type_usage):
        (type_usage,) = type_usage
        return type_usage

    def variable_declaration(self, identifier) -> VariableDeclaration:
        (identifier, type_usage) = identifier
        return VariableDeclaration(name=identifier, type=type_usage)

    def function_declaration_without_return(self, fdwr) -> FunctionDeclaration:
        return FunctionDeclaration(
            name=function[0],
            arguments=function[1:],
            return_type=DataTypeUsage(name=UNIT_TYPE),
            type=FunctionTypeUsage(
                arguments=[arg.type for arg in function[1:]],
                return_type=DataTypeUsage(name=UNIT_TYPE),
            ),
        )

    def function_declaration_with_return(self, fdwr) -> FunctionDeclaration:
        return Function(
            name=function[0],
            arguments=function[1:-1],
            return_type=function[-1],
            type=FunctionTypeUsage(
                arguments=[arg.type for arg in function[1:-1]], return_type=function[-1]
            ),
        )

    def function_declaration(self, fd) -> FunctionDeclaration:
        (fd,) = fd
        assert isinstance(fd, FunctionDeclaration)
        return fd

    def function(self, function) -> Function:
        return Function(
            declaration=function[0],
            block=function[1],
            type=UnknownTypeUsage(),
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

    def struct_definition_field(self, struct_definition_field):
        (field, type_usage) = struct_definition_field
        return (field, type_usage)

    def struct_type_declaration(self, struct_type_declaration) -> StructTypeDeclaration:
        name = struct_type_declaration[0]
        fields = {key: value for (key, value) in struct_type_declaration[1:]}
        return StructTypeDeclaration(name=name, fields=fields)

    def type_alias_declaration(self, type_alias_declaration) -> AliasTypeDeclaration:
        (name, existing) = type_alias_declaration
        return AliasTypeDeclaration(new=DataTypeUsage(name), old=type_alias_declaration)

    def type_declaration(self, type_declaration):
        (type_declaration,) = type_declaration
        return type_declaration

    def impl(self, impl) -> Impl:
        return Impl(struct=impl[0], functions=impl[1:])

    def module(self, module_items) -> Module:
        functions = []
        types = []
        impls = []
        for item in module_items:
            if isinstance(item, Function):
                functions.append(item)
            elif isinstance(item, Impl):
                impls.append(item)
            else:
                types.append(item)
        return Module(functions=functions, types=types, impls=impls)


boring_parser = Lark(boring_grammar, start="module", lexer="standard")

if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        tree = boring_parser.parse(f.read())
        # print(tree)
        result = TreeToBoring().transform(tree)
        pretty_print(result)
