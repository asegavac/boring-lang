import sys
from typing import List
from boring.parse import boring_parser, TreeToBoring, pretty_print
from boring.type_checking import TypeChecker, Context
from boring.type_alias_resolution import TypeAliasResolver, Context as AliasContex
from boring import typedefs, parse

builtins = {
    "U8": parse.PrimitiveTypeDeclaration("U8"),
    "U16": parse.PrimitiveTypeDeclaration("U16"),
    "U32": parse.PrimitiveTypeDeclaration("U32"),
    "U64": parse.PrimitiveTypeDeclaration("U64"),
    "U128": parse.PrimitiveTypeDeclaration("U128"),
    "I8": parse.PrimitiveTypeDeclaration("I8"),
    "I16": parse.PrimitiveTypeDeclaration("I16"),
    "I32": parse.PrimitiveTypeDeclaration("I32"),
    "I64": parse.PrimitiveTypeDeclaration("I64"),
    "I128": parse.PrimitiveTypeDeclaration("I128"),
    "F32": parse.PrimitiveTypeDeclaration("F32"),
    "F64": parse.PrimitiveTypeDeclaration("F64"),
    "F128": parse.PrimitiveTypeDeclaration("F128"),
    "()": parse.PrimitiveTypeDeclaration("()"),  # Unit
    "!": parse.PrimitiveTypeDeclaration("!"),  # Never
}

if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        tree = boring_parser.parse(f.read())
        # print(tree)
        result = TreeToBoring().transform(tree)
        # pretty_print(result)
        alias_resolver = TypeAliasResolver()
        alias_resolver.with_module(AliasContex([]), result)
        pretty_print(result)
        type_checker = TypeChecker()
        while type_checker.with_module(Context(builtins, None), result):
            print("loop")
        # type_checker.with_module({}, result)
        pretty_print(result)
        # tctb = TypeCheckTableBuilder()
        # table: List[TypeComparison] = []
        # tctb.with_module({}, table, result)
        # for e in table:
        #     print(e)
        # print("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
        # check_types(table)
        # print("vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
        # for e in table:
        #     print(e)

#        None, Some
#  None  skip, set
#  Some  set, check
