expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(logical(0))"));      
.Internal(formals(argv[[1]]));      
}, o=expected);      

