expected <- eval(parse(text="structure(list(dim = c(1L, 1L), a = c(NA, 3, -1, 2), class = structure(\"B\", package = \".GlobalEnv\")), .Names = c(\"dim\", \"a\", \"class\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(1, .Dim = c(1L, 1L), a = c(NA, 3, -1, 2), class = structure(\"B\", package = \".GlobalEnv\")))"));               
do.call(`attributes`, argv);               
}, o=expected);               

