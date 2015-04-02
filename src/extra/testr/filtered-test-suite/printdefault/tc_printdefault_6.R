expected <- eval(parse(text="quote(y ~ A:U + A:V - 1)"));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(y ~ A:U + A:V - 1), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

