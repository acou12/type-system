import std/strformat
import std/strutils

type TokenType* = enum
    Alpha,
    Operator,
    Punctuation,
    Number,
    White,
    Underscore,
    String,
    Comment,

type Token* = object
    tokenType*: TokenType
    index*: int
    value*: string

proc getLineFromIndex*(index: int, source: string): string = 
    if not (0 <= index and index < source.len):
        ""
    else:
        var back = index;
        while not (back == low(source) or source[back] == '\n'):
            dec back

        var front = index;
        while not (front == high(source) or source[front] == '\n'):
            inc front
        
        if front == high(source):
            inc front

        source[back..<front] & '\n' & ' '.repeat(index - back) & "^"

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
                    value: source[firstIndex..<index], tokenType: tokenType,
                    index: index,
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
                    value: source[index..index], tokenType: tokenType,
                    index: index,
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
            raise newException(OSError, "unmatched string.\n\n" & getLineFromIndex(index, source))
        tokens.add(
            Token(
                value: source[startIndex..<index], tokenType: TokenType.String,
                index: index,
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
            raise newException(OSError, "unmatched backquote id.\n\n" & getLineFromIndex(index, source))
        tokens.add(
            Token(
                value: source[startIndex..<index], tokenType: TokenType.Alpha,
                index: index,
            )
        )
        inc index
    else:
        result = false

proc lexComment(source: string, index: var int, tokens: var seq[Token]): bool =
    template isComment: bool = source[index..<index+2] == "//"
    if source.len - index >= 2 and isComment():
        result = true
        inc index, 2
        let startIndex = index
        while source.len - index >= 2 and not isComment():
            inc index
        if isComment():
            tokens.add(
                Token(
                    value: source[startIndex..<index], tokenType: TokenType.Comment,
                    index: startIndex
                )
            )
            inc index, 2
        else:
            raise newException(OSError, "unclosed comment.\n\n" & getLineFromIndex(startIndex, source))
    else:
        result = false

proc lex*(source: string): seq[Token] =
    let lexers = @[
        lexComment,
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
            raise newException(OSError, "lex error. invalid token at " & $index & "\n\n" & getLineFromIndex(index, source))

proc `$`*(token: Token): string =
    &"\"{token.value}\": {token.tokenType}"