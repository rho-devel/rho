expected <- eval(parse(text="quote(~a + b:c + d + e + e:d)"));     
test(id=0, code={     
argv <- eval(parse(text="list(quote(~a + b:c + d + e + e:d), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

