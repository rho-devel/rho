expected <- eval(parse(text="structure(numeric(0), .Dim = c(0L, 4L))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 4L)), structure(numeric(0), .Dim = c(0L, 4L)))"));      
do.call(`-`, argv);      
}, o=expected);      

