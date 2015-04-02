expected <- eval(parse(text="structure(\"0.01587\", .Names = \"(Intercept)\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(\"0.01587\", .Names = \"(Intercept)\"), NULL, FALSE, NULL, 2, FALSE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

