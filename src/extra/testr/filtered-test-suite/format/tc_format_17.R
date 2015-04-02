expected <- eval(parse(text="c(\"2.3e-05\", \"4.5e-06\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(2.3e-05, 4.5e-06), FALSE, 5L, 0L, NULL, 3L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

