expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 0L)))"));      
.Internal(formals(argv[[1]]));      
}, o=expected);      

