import lex
import utils

import tables
import strformat
import strutils
import sequtils
import sugar

type TypeExpressionType* = enum
    Number,
    String,
    Function,
    Void

type TypeExpression* = ref object
    case typeExpressionType: TypeExpressionType
    of Number: discard
    of String: discard
    of Function:
        params: seq[TypeExpression]
        returnType: TypeExpression
    of Void: discard

let voidType = TypeExpression(typeExpressionType: TypeExpressionType.Void)

type AstType* = enum
    Program,
    Assignment,
    Reassignment,
    FunctionCall,
    Id,
    NumberLiteral,
    StringLiteral,
    Function,
    Type,
    Any,

type Ast* = ref object
    typeExpression: TypeExpression
    case astType: AstType
    of AstType.Program:
        lines: seq[Ast]
    of AstType.Assignment:
        lhs: Ast
        lhsType: Ast
        rhs: Ast
    of AstType.Reassignment:
        reassignmentLhs: Ast
        reassignmentRhs: Ast
    of AstType.FunctionCall:
        function: Ast
        params: seq[Ast]
    of AstType.Id:
        name: string
    of AstType.NumberLiteral:
        number: int
    of AstType.StringLiteral:
        stringValue: string
    of AstType.Function:
        functionName: Ast
        functionParams: seq[tuple[name: Ast, `type`: Ast]]
        returnType: Ast
        body: seq[Ast]
    of AstType.Type:
        typeType: TypeExpression
    of AstType.Any: discard

let anyAst = Ast(astType: AstType.Any)

func `$`*(typeExpression: TypeExpression): string
func `$`*(ast: Ast): string

func `~=`(te1: TypeExpression, te2: TypeExpression): bool =
    case te1.typeExpressionType:
    of TypeExpressionType.Number:
        result = te2.typeExpressionType == TypeExpressionType.Number
    of TypeExpressionType.String:
        result = te2.typeExpressionType == TypeExpressionType.String
    of TypeExpressionType.Void:
        result = te2.typeExpressionType == TypeExpressionType.Void
    of TypeExpressionType.Function:
        result = te2.typeExpressionType == TypeExpressionType.Void and 
                 te2.returnType ~= te1.returnType
        for (p1, p2) in zip(te1.params, te2.params):
            result = result and p1 ~= p2

template `!~=`(te1: TypeExpression, te2: TypeExpression): bool =
    not (te1 ~= te2)

proc parse*(tokens: seq[Token]): Ast =
    var index = 0

    var typeTable = initTable[string, TypeExpression]()

    template currentToken: Token = tokens[index]

    proc error(message: string) =
        echo message
        var s = ""
        for i, token in pairs(tokens):
            if i == index:
                s &= ">>> "
                s &= token.value
                s &= " <<< "
            else:
                s &= token.value
                s &= " "
        echo s
        echo getStackTrace()
        quit(1)

    template next = index += 1
    template moreTokens: bool = index < tokens.len

    template hasToken(token: Token): bool = currentToken() == token
   
    proc expectToken(token: Token) =
        if not hasToken(token):
            error(fmt"invalid token: {currentToken()} at {index}")

    proc consumeToken(token: Token) =
        expectToken(token)
        next()
    
    template op(s: string): Token = Token(tokenType: TokenType.Operator, value: s)
    template punc(s: string): Token = Token(tokenType: TokenType.Punctuation, value: s)
    template alpha(s: string): Token = Token(tokenType: TokenType.Alpha, value: s)

    proc parseJoined(parser: proc (): Ast, sepToken: Token, endToken: Token): seq[Ast] =
        var first = true
        while not hasToken(endToken):
        
            if first:
                first = false
            else:
                consumeToken(sepToken)

            result.add(parser())

    proc parseJoinedPairs(parser: proc (): tuple[name: Ast, `type`: Ast], sepToken: Token, endToken: Token): seq[tuple[name: Ast, `type`: Ast]] =
        var first = true
        while not hasToken(endToken):
        
            if first:
                first = false
            else:
                consumeToken(sepToken)

            result.add(parser())

    proc parseProgram(): Ast
    proc parseLine(): Ast
    proc parseAssignment(): Ast
    proc parseVariableAssignment(lhs: Ast): Ast
    proc parseFunctionAssignment(lhs: Ast): Ast
    proc parseOperatorAssignment(): Ast
    proc parseExpression(): Ast
    proc parseBoundId(): Ast
    proc parseId(): Ast
    proc parseIdTypePair(): tuple[name: Ast, `type`: Ast]
    proc parseNum(): Ast
    proc parseString(): Ast
    proc parseType(): Ast

    proc parseProgram(): Ast =
        var lines: seq[Ast]

        while moreTokens():
            lines.add(parseLine())

        result = Ast(
            astType: AstType.Program,
            lines: lines,
            typeExpression: voidType,
        )

    proc parseLine(): Ast =
        if hasToken(alpha"let"):
            result = parseAssignment()
        else:
            result = parseExpression()
            consumeToken(punc";")
        
    proc parseAssignment(): Ast =
        consumeToken(alpha"let")
        if currentToken().tokenType == TokenType.Alpha:
            let lhs = parseBoundId()
            if typeTable.hasKey(lhs.name):
                error(fmt"{lhs.name} is already defined.")
            if hasToken(punc"("):
                result = parseFunctionAssignment(lhs)
            elif hasToken(op":"):
                result = parseVariableAssignment(lhs)
        elif hasToken(punc"("):
            result = parseOperatorAssignment()
        else:
            error("invalid assignment.")
        
    proc parseVariableAssignment(lhs: Ast): Ast =
        next() # skip the ":"
        let lhsType = parseType()
        var rhs: Ast
        if hasToken(op"="):
            next()
            rhs = parseExpression()
            if lhsType.typeType !~= rhs.typeExpression:
                error(fmt"a {rhs.typeExpression} is being assigned to {lhs.name}: {lhsType}")
            consumeToken(punc";")
        elif hasToken(punc";"):
            next()
            rhs = anyAst
        else:
            error("invalid assignment.")

        typeTable[lhs.name] = lhsType.typeType

        result = Ast(
            astType: AstType.Assignment,
            lhs: lhs,
            lhsType: lhsType,
            rhs: rhs,
            typeExpression: voidType,
        )

    proc parseFunctionAssignment(lhs: Ast): Ast =
        next()
        let params = parseJoinedPairs(parseIdTypePair, punc",", punc")")
        next()
        consumeToken(op":")
        let returnType = parseType()

        for (name, `type`) in params:
            if typeTable.hasKey(name.name):
                error(fmt"{name} is already defined.")
            else:
                typeTable[name.name] = `type`.typeType
        
        typeTable["result"] = returnType.typeType

        var body: seq[Ast]
        if hasToken(op"="):
            next()
            consumeToken(punc"{")
            while not hasToken(punc"}"):
                body.add(parseLine())
            next()
        elif hasToken(punc";"):
            body = @[anyAst]
            next()
        else:
            error("invalid assignment.")

        for (name, `type`) in params:
            typeTable.del(name.name)

        typeTable.del("result")

        var paramTypes: seq[TypeExpression]
        for (_, `type`) in params:
            paramTypes.add(`type`.typeType)

        typeTable[lhs.name] = TypeExpression(
            typeExpressionType: TypeExpressionType.Function,
            params: paramTypes,
            returnType: returnType.typeType
        )

        result = Ast(
            astType: AstType.Function,
            functionName: lhs,
            functionParams: params,
            returnType: returnType,
            body: body,
            typeExpression: voidType,
        )

    proc parseOperatorAssignment(): Ast =

        consumeToken(punc"(")
        let first = parseIdTypePair()
        let (firstId, firstType) = first
        consumeToken(punc")")

        let operator = currentToken()
        if operator.tokenType != TokenType.Operator:
            error("that should be an operator.")
        next()

        consumeToken(punc"(")
        let second = parseIdTypePair()
        let (secondId, secondType) = second
        consumeToken(punc")")

        if typeTable.hasKey(firstId.name):
            error(fmt"{firstId.name} is already defined")
        if typeTable.hasKey(secondId.name):
            error(fmt"{secondId.name} is already defined")
        
        typeTable[firstId.name] = firstType.typeType
        typeTable[secondId.name] = secondType.typeType

        consumeToken(op":")
        let returnType = parseType()

        typeTable["result"] = returnType.typeType

        var body: seq[Ast]
        if hasToken(op"="):
            next()
            consumeToken(punc"{")
            while not hasToken(punc"}"):
                body.add(parseLine())
            next()
        elif hasToken(punc";"):
            body = @[anyAst]
            next()
        else:
            error("invalid assignment.")
        
        typeTable.del(firstId.name)
        typeTable.del(secondId.name)
        typeTable.del("result")

        typeTable[operator.value] = TypeExpression(
            typeExpressionType: TypeExpressionType.Function,
            params: @[firstType.typeType, secondType.typeType],
            returnType: returnType.typeType
        )

        result = Ast(
            astType: AstType.Function,
            functionName: Ast(astType: AstType.Id, name: operator.value),
            functionParams: @[first, second],
            returnType: returnType,
            body: body,
            typeExpression: voidType,
        )

    proc functionApplicationType(f: TypeExpression, params: seq[TypeExpression]): TypeExpression =
        if f.typeExpressionType != TypeExpressionType.Function:
            error("not a function.")
        if params.len != f.params.len:
            error(fmt"{params} should be {f.params}")
        for index, param in pairs(params):
            if param !~= f.params[index]:
                error(fmt"{param} should be a {f.params[index]}")
        f.returnType

    # todo: for repeated calls
    # e.g., f(10)("a")(6) doesn't work right now
    # proc parseCalls(function: Ast): Ast

    proc parseExpression(): Ast =
        var firstExpression: Ast

        if hasToken(punc"("):
            next()
            firstExpression = parseExpression()
            consumeToken(punc")")
        elif currentToken().tokenType == TokenType.Number:
            firstExpression = parseNum()
        elif currentToken().tokenType == TokenType.String:
            firstExpression = parseString()
        elif currentToken().tokenType == TokenType.Alpha:
            firstExpression = parseId()
        else:
            error(fmt"invalid expression at {index}");

        if hasToken(op"="):
            next()
            let secondExpression = parseExpression()

            if firstExpression.typeExpression !~= secondExpression.typeExpression:
                error(fmt"{firstExpression.typeExpression} != {secondExpression.typeExpression}")
            
            result = Ast(
                astType: AstType.Reassignment,
                reassignmentLhs: firstExpression,
                reassignmentRhs: secondExpression,
                typeExpression: voidType,
            )
        elif currentToken().tokenType == TokenType.Operator:
            let operator = currentToken()
            if not typeTable.hasKey(operator.value):
                error(fmt"{operator.value} is not defined.")
            next()
            let secondExpression = parseExpression()

            let operatorAst = Ast(
                astType: Id,
                name: operator.value,
                typeExpression: typeTable[operator.value],
            )

            let params = @[firstExpression, secondExpression]

            let appType = functionApplicationType(operatorAst.typeExpression, params.map(param => param.typeExpression))

            result = Ast(
                astType: FunctionCall,
                function: operatorAst,
                params: params,
                typeExpression: appType,
            )
        elif hasToken(punc"("):
            next()

            let params = parseJoined(parseExpression, punc",", punc")")

            let appType = functionApplicationType(firstExpression.typeExpression, params.map(param => param.typeExpression))

            result = Ast(
                astType: FunctionCall,
                function: firstExpression,
                params: params,
                typeExpression: appType,
            )

            consumeToken(punc")")
        else: 
            result = firstExpression

    proc parseBoundId(): Ast =
        let idToken = currentToken()
        assert idToken.tokenType == TokenType.Alpha

        result = Ast(
            astType: Id,
            name: idToken.value,
        )

        next()

    proc parseId(): Ast =
        var id = parseBoundId()
        if not typeTable.hasKey(id.name):
            error(fmt"undefined variable ""{id}""")
        id.typeExpression = typeTable[id.name]
        result = id

    proc parseIdTypePair(): tuple[name: Ast, `type`: Ast] =
        (parseBoundId(), (consumeToken(op":"); parseType()))

    proc parseNum(): Ast =
        let numToken = currentToken()
        let number: int = numToken.value.parseInt()
        result = Ast(
            astType: NumberLiteral,
            number: number,
            typeExpression: TypeExpression(
                typeExpressionType: TypeExpressionType.Number,
            ),
        )
        next()

    proc parseString(): Ast =
        let stringToken = currentToken()
        result = Ast(
            astType: StringLiteral,
            stringValue: stringToken.value,
            typeExpression: TypeExpression(
                typeExpressionType: TypeExpressionType.String,
            ),
        )
        next()

    proc parseType(): Ast =
        if hasToken(alpha"number"):
            next()
            result = Ast(astType: AstType.Type, typeType: TypeExpression(typeExpressionType: TypeExpressionType.Number))
        elif hasToken(alpha"string"):
            next()
            result = Ast(astType: AstType.Type, typeType: TypeExpression(typeExpressionType: TypeExpressionType.String))
        elif hasToken(alpha"void"):
            next()
            result = Ast(astType: AstType.Type, typeType: TypeExpression(typeExpressionType: TypeExpressionType.Void))
        elif hasToken(alpha"fn"):
            next()
            consumeToken(punc"(")
            let params = parseJoined(parseType, punc",", punc")").map(ast => ast.typeType)
            next()
            consumeToken(op":")
            let returnType = parseType().typeType
            result = Ast(
                astType: AstType.Type, 
                typeType: TypeExpression(
                    typeExpressionType: TypeExpressionType.Function,
                    params: params,
                    returnType: returnType))
        elif hasToken(punc"("):
            next()
            result = parseType()
            consumeToken(punc")")
        else:
            error(fmt"invalid type: {currentToken()}")

    result = parseProgram()

func `$`*(typeExpression: TypeExpression): string =
    case typeExpression.typeExpressionType:
    of TypeExpressionType.Number: "number"
    of TypeExpressionType.String: "string"
    of TypeExpressionType.Function: fmt"""({typeExpression.params.join(" ")}) -> ({typeExpression.returnType})""" # TODO
    of TypeExpressionType.Void: "void"

func `$`*(ast: Ast): string =
    case ast.astType:
    of AstType.Program:
        for line in ast.lines:
            result &= $line
            result &= "\n"
    of AstType.Assignment:
        result = fmt"(let ({ast.lhs}: {ast.lhsType}) {ast.rhs})"
    of AstType.Reassignment:
        result = fmt"(relet {ast.reassignmentLhs} {ast.reassignmentRhs})"
    of AstType.FunctionCall:
        result = fmt"""({ast.function} {ast.params.join(" ")})"""
    of AstType.Id:
        result = ast.name
    of AstType.NumberLiteral:
        result = $ast.number
    of AstType.StringLiteral:
        result = "\"" & ast.stringValue & "\""
    of AstType.Function:
        var paramStrings: seq[string]
        for (name, `type`) in ast.functionParams:
            paramStrings.add(fmt"{name}: {`type`}")
        result = fmt"""(defn {ast.functionName} [{paramStrings.join(" ")}] -> [{ast.returnType}] {ast.body.join(" ")})"""
    of AstType.Type:
        result = $ast.typeType
    of AstType.Any:
        result = "any"