expected <- eval(parse(text="c(\" 1\", \" 2\", \" 3\", \" 4\", \" 5\", \"-1\", \"-2\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(1L, 2L, 3L, 4L, 5L, -1L, -2L), FALSE, NULL, 0L, NULL, 3L, FALSE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

