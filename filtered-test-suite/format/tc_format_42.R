expected <- eval(parse(text="c(\"1.1+0i\", \"    NA\", \"3.0+0i\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1.1+0i, NA, 3+0i), FALSE, NULL, 0L, NULL, 3L, FALSE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

