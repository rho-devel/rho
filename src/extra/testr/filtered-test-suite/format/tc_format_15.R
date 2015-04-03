expected <- eval(parse(text="\"2e-16\""));      
test(id=0, code={      
argv <- eval(parse(text="list(2.22044604925031e-16, FALSE, 1, 0L, NULL, 3L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

