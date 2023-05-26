template todo*(task: string) =
    template message: string = "TODO: " & task
    static: echo message()
    echo message()