import std/strformat

type TokenType* = enum
    Alpha,
    Operator,
    Punctuation,
    Number,
    White,
    Underscore,
    String,

type Token* = object
    tokenType*: TokenType
    value*: string

proc lexMultipleByFunction(
    accept: proc (c: char): bool,
    tokenType: TokenType): auto =
    proc (source: string, index: var int, tokens: var seq[Token]): bool =
        let firstIndex = index

        if accept(source[index]):
            while index < source.len and accept(source[index]):
                index += 1
            tokens.add(
                Token(
                    value: source[firstIndex..<index], tokenType: tokenType
                )
            )
            true
        else: false

proc lexSingleByFunction(
    accept: proc (c: char): bool,
    tokenType: TokenType): auto =
    proc (source: string, index: var int, tokens: var seq[Token]): bool =
        if accept(source[index]):
            tokens.add(
                Token(
                    value: source[index..index], tokenType: tokenType
                )
            )
            index += 1
            true
        else: false


let lexOp = lexMultipleByFunction(proc (c: char): bool = c in { '+', '-', '/', '*', '=', ':', '<', '>', '@', '~', '&', '%', '$', '.' }, TokenType.Operator)
let lexAlpha = lexMultipleByFunction(proc (c: char): bool = c in { 'a'..'z', 'A'..'Z', '-', '\'' }, TokenType.Alpha)
let lexPunc = lexSingleByFunction(proc (c: char): bool = c in { '[', ']', '(', ')', '{', '}', ',', ';' }, TokenType.Punctuation)
let lexNum = lexMultipleByFunction(proc (c: char): bool = c in { '0'..'9' }, TokenType.Number)
let lexWhite = lexMultipleByFunction(proc (c: char): bool = c in { ' ', '\n' }, TokenType.White)
let lexUnderscore = lexSingleByFunction(proc (c: char): bool = c in { '_' }, TokenType.Underscore)

proc lexString(source: string, index: var int, tokens: var seq[Token]): bool =
    if source[index] == '"':
        result = true
        inc index
        let startIndex = index
        while index < source.len and source[index] != '"':
            inc index
        if index >= source.len:
            echo "unmatched string."
            quit(1)
        tokens.add(
            Token(
                value: source[startIndex..<index], tokenType: TokenType.String
            )
        )
        inc index
    else:
        result = false

proc lexBackquotedId(source: string, index: var int, tokens: var seq[Token]): bool =
    if source[index] == '`':
        result = true
        inc index
        let startIndex = index
        while index < source.len and source[index] != '`':
            inc index
        if index >= source.len:
            echo "unmatched backquote id."
            quit(1)
        tokens.add(
            Token(
                value: source[startIndex..<index], tokenType: TokenType.Alpha
            )
        )
        inc index
    else:
        result = false


proc lex*(source: string): seq[Token] =
    let lexers = @[
        lexOp,
        lexAlpha,
        lexPunc,
        lexNum,
        lexWhite,
        lexUnderscore,
        lexString,
        lexBackquotedId,
    ]
    var index = 0
    while index < source.len:
        var found = false
        for lexer in lexers:
            if index >= source.len or lexer(source, index, result):
                found = true
                break
        if not found:
            echo "lex error. invalid token at ", index
            quit(1)

proc `$`*(token: Token): string =
    &"\"{token.value}\": {token.tokenType}"