expected <- eval(parse(text="structure(c(0, -1, -1, 0), class = \"table\", .Dim = c(2L, 2L), .Dimnames = structure(list(x = c(\"1\", \"2\"), y = c(\"1\", \"2\")), .Names = c(\"x\", \"y\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1L, 0L, 0L, 1L), .Dim = c(2L, 2L), .Dimnames = structure(list(x = c(\"1\", \"2\"), y = c(\"1\", \"2\")), .Names = c(\"x\", \"y\")), class = \"table\"), 1)"));      
do.call(`-`, argv);      
}, o=expected);      

