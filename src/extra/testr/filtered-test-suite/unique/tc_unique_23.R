expected <- eval(parse(text="c(1, 2, NA)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1, 2, NA, 2), FALSE, FALSE, NA)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

