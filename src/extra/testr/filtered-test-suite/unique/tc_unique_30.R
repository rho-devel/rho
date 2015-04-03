expected <- eval(parse(text="c(TRUE, FALSE)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(TRUE, FALSE, TRUE, TRUE), FALSE, FALSE, NA)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

