import sys
from typing import List
from boring.parse import boring_parser, TreeToBoring, pretty_print
from boring.type_checking import TypeChecker, Context
from boring import typedefs

if __name__ == "__main__":
    with open(sys.argv[1]) as f:
        tree = boring_parser.parse(f.read())
        # print(tree)
        result = TreeToBoring().transform(tree)
        # pretty_print(result)
        type_checker = TypeChecker()
        while type_checker.with_module(Context({}, typedefs.builtins, None), result):
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
