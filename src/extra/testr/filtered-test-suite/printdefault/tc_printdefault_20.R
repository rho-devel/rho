expected <- eval(parse(text="structure(c(NA, NA, 1L, 9L), .Names = c(\"size\", \"current\", \"direction\", \"eval_depth\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(NA, NA, 1L, 9L), .Names = c(\"size\", \"current\", \"direction\", \"eval_depth\")), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

