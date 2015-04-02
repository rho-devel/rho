expected <- eval(parse(text="logical(0)"));      
test(id=0, code={      
argv <- eval(parse(text="list(character(0), TRUE)"));      
.Internal(file.create(argv[[1]], argv[[2]]));      
}, o=expected);      

