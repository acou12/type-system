import compile

proc main =
    let compiled, _ = compile(readFile("src/index.lang"))

    var file: File
    if open(file, "out.js", fmWrite):
        file.write(compiled)
    else:
        echo "error writing."

main()