expected <- eval(parse(text="\"\\abc\""));      
test(id=0, code={      
argv <- eval(parse(text="list(\"\\abc\", FALSE, NULL, 0L, NULL, 3L, FALSE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

