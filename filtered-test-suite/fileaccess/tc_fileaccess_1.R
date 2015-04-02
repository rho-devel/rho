expected <- eval(parse(text="integer(0)"));      
test(id=0, code={      
argv <- eval(parse(text="list(character(0), 0)"));      
.Internal(file.access(argv[[1]], argv[[2]]));      
}, o=expected);      

