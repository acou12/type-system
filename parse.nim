import lex
import strformat
import strutils
import parseutils

type AstType* = enum
    Program,
    Assignment,
    FunctionCall,
    Id,
    NumberLiteral,
    Function,

type Ast* = ref object
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

proc parse*(tokens: seq[Token]): Ast =
    var index = 0

    proc currentToken: Token = tokens[index]

    proc error(message: string) =
        echo message
        echo getStackTrace()
        quit(1)

    template next = index += 1
    template moreTokens: bool = index < tokens.len
   
    proc expectToken(token: Token) =
        if currentToken() != token:
            error(fmt"invalid token: {currentToken()} at {index}")

    proc consumeToken(token: Token) =
        expectToken(token)
        next()
    
    template hasToken(token: Token): bool = currentToken() == token

    template op(s: string): Token = Token(tokenType: TokenType.Operator, value: s)
    template punc(s: string): Token = Token(tokenType: TokenType.Punctuation, value: s)
    template alpha(s: string): Token = Token(tokenType: TokenType.Alpha, value: s)

    proc parseJoined[T](parser: proc (): T, sepToken: Token, endToken: Token): seq[T] =
        var first = true
        while currentToken() != endToken:
        
            if first:
                first = false
            else:
                consumeToken(sepToken)

            result.add(parser())

    proc parseProgram(): Ast
    proc parseLine(): Ast
    proc parseAssignment(): Ast
    proc parseExpression(): Ast
    proc parseId(): Ast
    proc parseIdTypePair(): tuple[name: Ast, `type`: Ast]
    proc parseNum(): Ast

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
        let lhs = parseId()
        if currentToken() == punc"(":
            next()
            let params = parseJoined(parseIdTypePair, punc",", punc")")
            next()
            consumeToken(op":")
            let returnType = parseId()
            consumeToken(op"=")
            consumeToken(punc"{")
            var body: seq[Ast]
            while currentToken() != punc"}":
                body.add(parseLine())
            next()
            result = Ast(
                astType: AstType.Function,
                functionName: lhs,
                functionParams: params,
                returnType: returnType,
                body: body
            )
        elif currentToken() == op":":
            next()
            let lhsType = parseId()
            consumeToken(op"=")
            let rhs = parseExpression()
            consumeToken(punc";")
            result = Ast(
                astType: AstType.Assignment,
                lhs: lhs,
                lhsType: lhsType,
                rhs: rhs,
            )
        
    proc parseExpression(): Ast =
        var firstExpression: Ast

        if currentToken() == punc"(":
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
            result = Ast(
                astType: FunctionCall,
                function: Ast(
                    astType: Id,
                    name: operator.value
                ),
                params: @[firstExpression, secondExpression]
            )
        elif currentToken() == punc"(":
            next()

            let params = parseJoined(parseExpression, punc",", punc")")

            result = Ast(
                astType: FunctionCall,
                function: firstExpression,
                params: params
            )

            consumeToken(punc")")
        else: 
            result = firstExpression

    proc parseId(): Ast =
        let idToken = currentToken()
        assert idToken.tokenType == TokenType.Alpha
        result = Ast(
            astType: Id,
            name: idToken.value
        )
        next()

    proc parseIdTypePair(): tuple[name: Ast, `type`: Ast] =
        (parseId(), (consumeToken(op":"); parseId()))

    proc parseNum(): Ast =
        let numToken = currentToken()
        var number: int
        discard numToken.value.parseInt(number)
        result = Ast(
            astType: NumberLiteral,
            number: number,
        )
        next()
    
    result = parseProgram()

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
        result = fmt"""(defn {ast.functionName} [{paramStrings.join(" ")}] {ast.body.join(" ")})"""