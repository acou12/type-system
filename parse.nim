import lex
import utils

import tables
import strformat
import strutils
import sequtils
import sugar

type TypeExpressionType* = enum
    Number,
    Function,
    Void

type TypeExpression* = ref object
    case typeExpressionType: TypeExpressionType
    of Number: discard
    of Function:
        params: seq[TypeExpression]
        returnType: TypeExpression
    of Void: discard

let voidType = TypeExpression(typeExpressionType: TypeExpressionType.Void)

type AstType* = enum
    Program,
    Assignment,
    FunctionCall,
    Id,
    NumberLiteral,
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
    of AstType.FunctionCall:
        function: Ast
        params: seq[Ast]
    of AstType.Id:
        name: string
    of AstType.NumberLiteral:
        number: int
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
    proc parseExpression(): Ast
    proc parseBoundId(): Ast
    proc parseId(): Ast
    proc parseIdTypePair(): tuple[name: Ast, `type`: Ast]
    proc parseNum(): Ast
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
            elif hasToken(op":"):
                next()
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
        else:
            error("invalid assignment.")

    proc functionApplicationType(f: TypeExpression, params: seq[TypeExpression]): TypeExpression =
        if f.typeExpressionType != TypeExpressionType.Function:
            error("not a function.")
        if params.len != f.params.len:
            error(fmt"{params} should be {f.params}")
        for index, param in pairs(params):
            if param !~= f.params[index]:
                error(fmt"{param} should be a {f.params[index]}")
        f.returnType

    proc parseExpression(): Ast =
        var firstExpression: Ast

        if hasToken(punc"("):
            next()
            firstExpression = parseExpression()
            consumeToken(punc")")
        elif currentToken().tokenType == TokenType.Number:
            firstExpression = parseNum()
        elif currentToken().tokenType == TokenType.Alpha:
            firstExpression = parseId()
        else:
            error(fmt"invalid expression at {index}");

        if currentToken().tokenType == TokenType.Operator:
            let operator = currentToken()
            next()
            let secondExpression = parseExpression()

            todo("operators do not type check")

            let operatorAst = Ast(
                astType: Id,
                name: operator.value,
                # typeExpression: 
            )

            result = Ast(
                astType: FunctionCall,
                function: operatorAst,
                params: @[firstExpression, secondExpression],
                # typeExpression: functionType(),
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

    proc parseType(): Ast =
        if hasToken(alpha"number"):
            next()
            result = Ast(astType: AstType.Type, typeType: TypeExpression(typeExpressionType: TypeExpressionType.Number))
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
    echo typeTable

func `$`*(typeExpression: TypeExpression): string =
    case typeExpression.typeExpressionType:
    of TypeExpressionType.Number: "number"
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
    of AstType.FunctionCall:
        result = fmt"""({ast.function} {ast.params.join(" ")})"""
    of AstType.Id:
        result = ast.name
    of AstType.NumberLiteral:
        result = $ast.number
    of AstType.Function:
        var paramStrings: seq[string]
        for (name, `type`) in ast.functionParams:
            paramStrings.add(fmt"{name}: {`type`}")
        result = fmt"""(defn {ast.functionName} [{paramStrings.join(" ")}] -> [{ast.returnType}] {ast.body.join(" ")})"""
    of AstType.Type:
        result = $ast.typeType
    of AstType.Any:
        result = "any"