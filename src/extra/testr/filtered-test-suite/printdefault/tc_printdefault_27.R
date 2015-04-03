expected <- eval(parse(text="c(TRUE, TRUE, TRUE)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(TRUE, TRUE, TRUE), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

