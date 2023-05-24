import lex
import parse
import sugar

proc main =
    let entryPointContents = readFile("index.lang")

    var tokens = collect(
        for token in lex(entryPointContents):
            if token.tokenType != TokenType.White:
                token)

    let ast = parse(tokens)

    echo ast

main()