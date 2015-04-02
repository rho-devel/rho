expected <- eval(parse(text="structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(10L, 1L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"), NULL))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(10L, 0L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"), NULL)), structure(numeric(0), .Names = character(0)))"));      
do.call(`%*%`, argv);      
}, o=expected);      

