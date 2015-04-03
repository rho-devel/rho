expected <- eval(parse(text="structure(NA, .Dim = c(1L, 1L), name = \"Sam\", class = structure(\"Foo\", package = \".GlobalEnv\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(NA, .Dim = c(1L, 1L), name = \"Sam\", class = structure(\"Foo\", package = \".GlobalEnv\")))"));               
do.call(`(`, argv);               
}, o=expected);               

