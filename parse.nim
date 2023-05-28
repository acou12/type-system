import lex

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
    AnonFunction,
    Type,
    Any,
    Extern,

type Ast* = ref object
    typeExpression*: TypeExpression
    case astType*: AstType
    of AstType.Program:
        lines*: seq[Ast]
    of AstType.Assignment:
        lhs*: Ast
        lhsType*: Ast
        rhs*: Ast
    of AstType.Reassignment:
        reassignmentLhs*: Ast
        reassignmentRhs*: Ast
    of AstType.FunctionCall:
        function*: Ast
        params*: seq[Ast]
    of AstType.Id:
        name*: string
    of AstType.NumberLiteral:
        number*: int
    of AstType.StringLiteral:
        stringValue*: string
    of AstType.Function:
        functionName*: Ast
        functionParams*: seq[tuple[name: Ast, `type`: Ast]]
        returnType*: Ast
        body*: seq[Ast]
    of AstType.AnonFunction:
        anonFunctionParams*: seq[tuple[name: Ast, `type`: Ast]]
        anonReturnType*: Ast
        anonBody*: seq[Ast]
    of AstType.Type:
        typeType*: TypeExpression
    of AstType.Any: discard
    of AstType.Extern:
        externCode*: Ast
        externType*: Ast

let anyAst = Ast(astType: AstType.Any)

func `$`*(typeExpression: TypeExpression): string
func `$`*(ast: Ast): string

# structural type equivalence
func `~=`(te1: TypeExpression, te2: TypeExpression): bool =
    case te1.typeExpressionType:
    of TypeExpressionType.Number:
        result = te2.typeExpressionType == TypeExpressionType.Number
    of TypeExpressionType.String:
        result = te2.typeExpressionType == TypeExpressionType.String
    of TypeExpressionType.Void:
        result = te2.typeExpressionType == TypeExpressionType.Void
    of TypeExpressionType.Function:
        result = te2.typeExpressionType == TypeExpressionType.Function and 
                 te2.returnType ~= te1.returnType
        for (p1, p2) in zip(te1.params, te2.params):
            result = result and p1 ~= p2

template `!~=`(te1: TypeExpression, te2: TypeExpression): bool =
    not (te1 ~= te2)

proc parse*(tokens: seq[Token]): Ast =
    var index = 0

    var typeTable = initTable[string, TypeExpression]()

    template currentToken(useIndex = index): Token = tokens[useINdex]

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

    template hasToken(token: Token, useIndex = index): bool = currentToken(useIndex) == token
   
    proc expectToken(token: Token) =
        if not hasToken(token):
            error(fmt"invalid token: {currentToken()} at {index}. expected {token}.")

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
    proc parseExtern(): Ast
    proc parseBoundId(): Ast
    proc parseId(): Ast
    proc parseIdTypePair(): tuple[name: Ast, `type`: Ast]
    proc parseNum(): Ast
    proc parseString(): Ast
    proc parseType(): Ast
    proc parseAnonFunction(): Ast

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
            elif hasToken(op":") or hasToken(op"="):
                result = parseVariableAssignment(lhs)
        elif hasToken(punc"("):
            result = parseOperatorAssignment()
        else:
            error("invalid assignment.")
        
    proc parseVariableAssignment(lhs: Ast): Ast =
        var lhsType: Ast
        if hasToken(op":"):
            next()
            lhsType = parseType()
        var rhs: Ast
        if hasToken(op"="):
            next()
            rhs = parseExpression()
            if not lhsType.isNil and lhsType.typeType !~= rhs.typeExpression:
                error(fmt"a {rhs.typeExpression} is being assigned to {lhs.name}: {lhsType}")
            consumeToken(punc";")
        elif hasToken(punc";"):
            next()
            rhs = anyAst
        else:
            error("invalid assignment.")

        typeTable[lhs.name] = rhs.typeExpression

        result = Ast(
            astType: AstType.Assignment,
            lhs: lhs,
            lhsType: if lhsType.isNil: Ast(astType: AstType.Type, typeType: rhs.typeExpression) else: lhsType,
            rhs: rhs,
            typeExpression: voidType,
        )

    proc parseFunctionAssignment(lhs: Ast): Ast =
        if typeTable.hasKey(lhs.name):
            error(fmt"{lhs.name} is already defined")

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
            consumeToken(punc";")
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
        if typeTable.hasKey(operator.value):
            error(fmt"{operator.value} is already defined")

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
        elif hasToken(op"@"):
            firstExpression = parseExtern()
        elif hasToken(punc"{"):
            firstExpression = parseAnonFunction()
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
    
    proc parseExtern(): Ast =
        next()
        consumeToken(punc"{")
        let externCode = parseString()
        consumeToken(op":")
        let externType = parseType()
        consumeToken(punc"}")
        
        result = Ast(
            astType: AstType.Extern,
            externCode: externCode,
            externType: externType,
            typeExpression: externType.typeType,
        )

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
        elif hasToken(punc"("):
            next()
            result = parseType()
            if hasToken(punc","):
                next()
                let params = parseJoined(parseType, punc",", punc")").map(ast => ast.typeType)
                next()
                consumeToken(op"->")
                let returnType = parseType()
                result = Ast(
                    astType: AstType.Type, 
                    typeType: TypeExpression(
                        typeExpressionType: TypeExpressionType.Function,
                        params: @[result.typeType] & params,
                        returnType: returnType.typeType))
            else:
                consumeToken(punc")")
        else:
            error(fmt"invalid type: {currentToken()}")
            nil

        if hasToken(op"->"):
            next()
            let returnType = parseType()
            result = Ast(
                astType: AstType.Type, 
                typeType: TypeExpression(
                    typeExpressionType: TypeExpressionType.Function,
                    params: @[result.typeType],
                    returnType: returnType.typeType))
    
    # proc anonFunctionHasParams(): bool =
    #     var tempIndex = index
    #     result = true
    #     while result and not hasToken(op":"):
    #         if not (
    #             currentToken(tempIndex).tokenType == TokenType.Alpha and (hasToken(punc",", tempIndex + 1) or hasToken(op"=>", tempIndex + 1))
    #         ): result = false
    #         inc tempIndex, 2
    
    proc parseAnonFunction(): Ast =
        consumeToken(punc"{")

        var params: seq[tuple[name: Ast, `type`: Ast]]
        while not hasToken(op":"):
            params.add(parseIdTypePair())
            if not hasToken(op":"):
                consumeToken(punc",")
        next()
        var returnType = parseType()
        
        consumeToken(op"=>")

        for param in params:
            typeTable[param[0].name] = param[1].typeType
        
        typeTable["result"] = returnType.typeType

        var lines: seq[Ast]
        while not hasToken(punc"}"):
            lines.add(parseLine())
        next()

        for param in params:
            typeTable.del(param[0].name)

        typeTable.del("result")

        result = Ast(
            astType: AstType.AnonFunction,
            anonFunctionParams: params,
            anonReturnType: returnType,
            anonBody: lines,
            typeExpression: TypeExpression(
                typeExpressionType: TypeExpressionType.Function,
                params: params.map(param => param[1].typeType),
                returnType: returnType.typeType,
            )
        )

    result = parseProgram()

func `$`*(typeExpression: TypeExpression): string =
    case typeExpression.typeExpressionType:
    of TypeExpressionType.Number: "number"
    of TypeExpressionType.String: "string"
    of TypeExpressionType.Function: fmt"""({typeExpression.params.join(" ")}) -> ({typeExpression.returnType})""" # TODO
    of TypeExpressionType.Void: "void"

proc encodeChar(c: char): string =
    let numString = ord(c).intToStr
    for c in numString:
        result &= cast[char]((cast[byte](c) - cast[byte]('0')) + cast[byte]('a'))

func `$`*(ast: Ast): string =
    case ast.astType:
    of AstType.Program:
        for line in ast.lines:
            result &= $line
            result &= "\n"
    of AstType.Assignment:
        result = fmt"let {ast.lhs} = {ast.rhs};"
    of AstType.Reassignment:
        result = fmt"{ast.reassignmentLhs} = {ast.reassignmentRhs}"
    of AstType.FunctionCall:
        if ast.function.name == "js" and ast.params[0].astType == AstType.StringLiteral:
            result = fmt"""{ast.params[0].stringValue}"""
        else:
            result = fmt"""{ast.function}({ast.params.join(", ")})"""
    of AstType.AnonFunction:
        var paramStrings: seq[string]
        for (name, _) in ast.anonFunctionParams:
            paramStrings.add(fmt"{name}")
        result = fmt"""function ({paramStrings.map(param => param[0]).join(", ")})""" 
        result &= "{"
        result &= "let result;"
        for line in ast.anonBody:
            result &= $line & ";"
        result &= "return result;"
        result &= "}"
    of AstType.Id:
        for c in ast.name:
            if not (('a' <= c and c <= 'z') or ('A' <= c and c <= 'Z')):
                result &= "__op__" & encodeChar(c)
            else:
                result &= c
    of AstType.NumberLiteral:
        result = $ast.number
    of AstType.StringLiteral:
        result = "\"" & ast.stringValue & "\""
    of AstType.Function:
        var paramStrings: seq[string]
        for (name, _) in ast.functionParams:
            paramStrings.add(fmt"{name}")
        result = fmt"""function {ast.functionName}({paramStrings.join(", ")})""" 
        result &= "{"
        result &= "let result;"
        for line in ast.body:
            result &= $line & ";"
        result &= "return result;"
        result &= "}"
    of AstType.Type:
        result = $ast.typeType;
    of AstType.Any:
        result = ""
    of AstType.Extern:
        result = ast.externCode.stringValue