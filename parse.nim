import lex
import strformat
import parseutils

type AstType = enum
    Program,
    Assignment,
    FunctionCall,
    Id,
    NumberLiteral,

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

    proc parseProgram(): Ast
    proc parseLine(): Ast
    proc parseAssignment(): Ast
    proc parseExpression(): Ast
    proc parseId(): Ast
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
        if currentToken() == Token(tokenType: TokenType.Alpha, value: "let"):
            result = parseAssignment()
        else:
            result = parseExpression()
            consumeToken(Token(tokenType: TokenType.Punctuation, value: ";"))

    proc parseAssignment(): Ast =
        consumeToken(Token(tokenType: TokenType.Alpha, value: "let"))
        let lhs = parseId()
        consumeToken(Token(tokenType: TokenType.Operator, value: ":"))
        let lhsType = parseId()
        consumeToken(Token(tokenType: TokenType.Operator, value: "="))
        let rhs = parseExpression()
        consumeToken(Token(tokenType: TokenType.Punctuation, value: ";"))
        result = Ast(
            astType: AstType.Assignment,
            lhs: lhs,
            lhsType: lhsType,
            rhs: rhs,
        )

    proc parseExpression(): Ast =
        var firstExpression: Ast

        if currentToken() == Token(tokenType: TokenType.Punctuation, value: "("):
            next()
            firstExpression = parseExpression()
            consumeToken(Token(tokenType: TokenType.Punctuation, value: ")"))
        elif currentToken().tokenType == TokenType.Number:
            firstExpression = parseNum()
        elif currentToken().tokenType == TokenType.Alpha:
            firstExpression = parseId()

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
        elif currentToken() == Token(tokenType: TokenType.Punctuation, value: "("):
            next()

            var params: seq[Ast]
            var firstParam = true
            while currentToken() != Token(tokenType: TokenType.Punctuation, value: ")"):
                if not firstParam:
                    consumeToken(Token(tokenType: TokenType.Punctuation, value: ","))
                else:
                    firstParam = false
                params.add(parseExpression())

            result = Ast(
                astType: FunctionCall,
                function: firstExpression,
                params: params
            )

            consumeToken(Token(tokenType: TokenType.Punctuation, value: ")"))
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

proc `$`*(ast: Ast): string =
    case ast.astType:
    of AstType.Program:
        for line in ast.lines:
            result &= $line
            result &= "\n"
    of AstType.Assignment:
        result = fmt"(let ({ast.lhs}: {ast.lhsType}) {ast.rhs})"
    of AstType.FunctionCall:
        result = fmt"({ast.function} "
        var first = true
        for param in ast.params:
            if first:
                first = false
            else:
                result &= " "
            result &= $param
        result &= ")"
    of AstType.Id:
        result = ast.name
    of AstType.NumberLiteral:
        result = $ast.number