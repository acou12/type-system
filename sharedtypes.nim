import strformat
import strutils

type TEType* = enum
    Basic,
    Variable,
    Function

type BasicType* = enum
    Integer,
    String,
    Void

type TE* = ref object
    case teType*: TEType
    of Basic:
        basicType*: BasicType
    of Variable:
        variableId*: int
    of Function:
        functionParams*: seq[TE]
        functionReturn*: TE

func `$`*(te: TE): string =
    case te.teType:
    of TEType.Basic:
        case te.basicType:
        of BasicType.Integer:
            "number"
        of BasicType.String:
            "string"
        of BasicType.Void:
            "void"
    of TEType.Function:
        fmt"""({te.functionParams.join(" ")}) -> ({te.functionReturn})""" # TODO
    of TEType.Variable:
        "'" & $chr(te.variableId) 
