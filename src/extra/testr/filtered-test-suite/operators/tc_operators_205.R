expected <- eval(parse(text="structure(c(1L, 2L, 1L), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\"), class = \"table\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(1L, 2L, 1L), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\"), class = \"table\"))"));     
do.call(`(`, argv);     
}, o=expected);     

