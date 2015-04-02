expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(NA, 2, NA, 1, NA, 0), .Dim = 2:3), structure(c(NA, 2, NA, 1, NA, 0), .Dim = 2:3), TRUE, TRUE, TRUE, TRUE, FALSE)"));      
.Internal(`identical`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));      
}, o=expected);      

