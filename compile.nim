import lex
import parse

import sugar

const prelude = readFile("src/prelude.lang")

proc compile*(source: cstring): (cstring, bool) {.exportc} =
    let contents = prelude & $source

    try:
        let tokens = collect(
            for token in lex(contents):
                if not (token.tokenType in { TokenType.White, TokenType.Comment }):
                    token)
        
        let ast = parse(tokens, contents)

        let cstringAst: cstring = $ast

        result = (cstringAst, true)

    except OSError:
        let cstringMsg: cstring = getCurrentExceptionMsg()
        result = (cstringMsg, false)