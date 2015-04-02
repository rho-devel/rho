expected <- eval(parse(text="list(c(9L, 9L))"));      
test(id=0, code={      
argv <- eval(parse(text="list(list(c(9L, 9L), c(9L, 9L), c(9L, 9L), c(9L, 9L), c(9L, 9L), c(9L, 9L)), FALSE, FALSE, NA)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

