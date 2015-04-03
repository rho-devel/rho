expected <- eval(parse(text="c(\"NA\", \" 2\", \" 4\", \" 7\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(NA, 2L, 4L, 7L), FALSE, NULL, 0L, NULL, 3L, FALSE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

