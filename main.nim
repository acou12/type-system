import lex
import parse

import sugar

proc compile*(code: cstring): cstring {.exportc.} =
    const prelude = static: readFile("src/prelude.lang")
    let entryPointContents = prelude & $code
    echo entryPointContents

    var tokens = collect(
        for token in lex(entryPointContents):
            if token.tokenType != TokenType.White:
                token)

    let ast = parse(tokens)

    result = $ast