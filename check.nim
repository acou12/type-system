import sharedtypes

import sugar
import parse
import sets
import tables

type AttributeSet = object
    te: TE
    # a set of required equalities (first = second)
    constraints: HashSet[tuple[first: TE, second: TE]]

proc error() =
    echo getStackTrace()
    quit(1)

proc type(ast: Ast, symbolTable: var Table[string, AttributeSet]): TE =
    case ast.astType
    of AstType.Id:
        symbolTable[ast.name]
    of AstType.Assignment:
        let attrSet = AttributeSet(te: ast.lhsType.typeType)

        let rhsAttr = typeExpression(ast.rhs, symbolTable)
        let rhsType = rhsAttr.te
        if rhsType != attrSet.te:
            error()

        symbolTable[ast.lhs.name] = attrSet
        nil
    else: nil

let x = 1;
let y = "a";
let f = { z =>
    result = z + x;
};


WHAT AM I DOING