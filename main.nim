import lex
import parse

import sugar

proc main =
    let entryPointContents = readFile("src/prelude.lang") & readFile("src/index.lang")

    var tokens = collect(
        for token in lex(entryPointContents):
            if token.tokenType != TokenType.White:
                token)

    let ast = parse(tokens)

    var file: File
    if open(file, "out.js", fmWrite):
        file.write($ast)
    else:
        echo "error writing."

main()
