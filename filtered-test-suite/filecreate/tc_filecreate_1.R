expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"codetools-manual.log\", TRUE)"));      
.Internal(file.create(argv[[1]], argv[[2]]));      
}, o=expected);      

