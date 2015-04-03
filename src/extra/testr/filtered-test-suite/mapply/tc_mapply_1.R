expected <- eval(parse(text="list()"));      
test(id=0, code={      
argv <- eval(parse(text="list(.Primitive(\"c\"), list(list(), list(), list()), NULL)"));      
.Internal(mapply(argv[[1]], argv[[2]], argv[[3]]));      
}, o=expected);      

