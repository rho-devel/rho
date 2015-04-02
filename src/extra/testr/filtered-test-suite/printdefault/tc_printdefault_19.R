expected <- eval(parse(text="c(\"2007-11-06\", \"2007-11-06\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"2007-11-06\", \"2007-11-06\"), NULL, TRUE, NULL, NULL, FALSE, 99999L, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

