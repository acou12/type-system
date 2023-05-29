import lex
import sharedtypes

import tables
import strformat
import strutils
import sequtils
import sugar
import macros

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
        typeType*: TE
    of AstType.Any: discard
    of AstType.Extern:
        externCode*: Ast

let anyAst = Ast(astType: AstType.Any)

func `$`*(ast: Ast): string

proc parse*(tokens: seq[Token]): Ast =
    var index = 0

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
     
    proc consumeToken(token: Token, skip: static bool = false) =
        if not skip:
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
        if lhsType.isNil:
            error("type inference is not supported.")
        var rhs: Ast
        if hasToken(op"="):
            next()
            rhs = parseExpression()
            consumeToken(punc";")
        elif hasToken(punc";"):
            next()
            rhs = anyAst
        else:
            error("invalid assignment.")

        result = Ast(
            astType: AstType.Assignment,
            lhs: lhs,
            lhsType: lhsType,
            rhs: rhs,
        )

    proc parseFunctionAssignment(lhs: Ast): Ast =
        next()
        let params = parseJoinedPairs(parseIdTypePair, punc",", punc")")
        next()
        consumeToken(op":")
        let returnType = parseType()

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

        var paramTypes: seq[TE]
        for (_, `type`) in params:
            paramTypes.add(`type`.typeType)

        result = Ast(
            astType: AstType.Function,
            functionName: lhs,
            functionParams: params,
            returnType: returnType,
            body: body,
        )

    proc parseOperatorAssignment(): Ast =

        consumeToken(punc"(")
        let first = parseIdTypePair()
        consumeToken(punc")")

        let operator = currentToken()
        if operator.tokenType != TokenType.Operator:
            error("that should be an operator.")

        next()

        consumeToken(punc"(")
        let second = parseIdTypePair()
        consumeToken(punc")")

        consumeToken(op":")
        let returnType = parseType()

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
        
        result = Ast(
            astType: AstType.Function,
            functionName: Ast(astType: AstType.Id, name: operator.value),
            functionParams: @[first, second],
            returnType: returnType,
            body: body,
        )

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

            result = Ast(
                astType: AstType.Reassignment,
                reassignmentLhs: firstExpression,
                reassignmentRhs: secondExpression,
            )
        elif currentToken().tokenType == TokenType.Operator:
            let operator = currentToken()
            next()
            let secondExpression = parseExpression()

            let operatorAst = Ast(
                astType: Id,
                name: operator.value,
            )

            let params = @[firstExpression, secondExpression]

            result = Ast(
                astType: FunctionCall,
                function: operatorAst,
                params: params,
            )
        elif hasToken(punc"("):
            next()

            let params = parseJoined(parseExpression, punc",", punc")")

            result = Ast(
                astType: FunctionCall,
                function: firstExpression,
                params: params,
            )

            consumeToken(punc")")
        else: 
            result = firstExpression
    
    proc parseExtern(): Ast =
        next()
        consumeToken(punc"{")
        let externCode = parseString()
        consumeToken(punc"}")
        
        result = Ast(
            astType: AstType.Extern,
            externCode: externCode,
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
        result = parseBoundId()

    proc parseIdTypePair(): tuple[name: Ast, `type`: Ast] =
        (parseBoundId(), (consumeToken(op":"); parseType()))

    proc parseNum(): Ast =
        let numToken = currentToken()
        let number: int = numToken.value.parseInt()
        result = Ast(
            astType: NumberLiteral,
            number: number,
        )
        next()

    proc parseString(): Ast =
        let stringToken = currentToken()
        result = Ast(
            astType: StringLiteral,
            stringValue: stringToken.value,
        )
        next()

    proc parseType(): Ast =
        if hasToken(alpha"number"):
            next()
            result = Ast(astType: AstType.Type, typeType: TE(teType: TEType.Basic, basicType: BasicType.Integer))
        elif hasToken(alpha"string"):
            next()
            result = Ast(astType: AstType.Type, typeType: TE(teType: TEType.Basic, basicType: BasicType.String))
        elif hasToken(alpha"void"):
            next()
            result = Ast(astType: AstType.Type, typeType: TE(teType: TEType.Basic, basicType: BasicType.Void))
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
                    typeType: TE(
                        teType: TEType.Function,
                        functionParams: @[result.typeType] & params,
                        functionReturn: returnType.typeType))
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
                typeType: TE(
                    teType: TEType.Function,
                    functionParams: @[result.typeType],
                    functionReturn: returnType.typeType))
   
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

        var lines: seq[Ast]
        while not hasToken(punc"}"):
            lines.add(parseLine())
        next()

        result = Ast(
            astType: AstType.AnonFunction,
            anonFunctionParams: params,
            anonReturnType: returnType,
            anonBody: lines,
        )

    result = parseProgram()

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