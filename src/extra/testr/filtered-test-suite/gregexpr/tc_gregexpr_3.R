expected <- eval(parse(text="list(structure(8L, match.length = 1L, useBytes = TRUE))"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"$\", \"version$m\", FALSE, FALSE, TRUE, FALSE)"));      
.Internal(gregexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));      
}, o=expected);      

