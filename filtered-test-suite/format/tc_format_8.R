expected <- eval(parse(text="\"NA\""));      
test(id=0, code={      
argv <- eval(parse(text="list(NA_real_, FALSE, 4L, 0L, NULL, 3L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

