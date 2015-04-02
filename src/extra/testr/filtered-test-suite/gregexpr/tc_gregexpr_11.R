expected <- eval(parse(text="list()"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"[[:space:]]?(,|,?[[:space:]]and)[[:space:]]+\", character(0), FALSE, FALSE, FALSE, FALSE)"));      
.Internal(gregexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));      
}, o=expected);      

