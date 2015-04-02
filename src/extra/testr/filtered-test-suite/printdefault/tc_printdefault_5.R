expected <- eval(parse(text="structure(c(-1.05715, -0.48359, 0.0799, 0.44239, 1.2699), .Names = c(\"Min\", \"1Q\", \"Median\", \"3Q\", \"Max\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(-1.05715, -0.48359, 0.0799, 0.44239, 1.2699), .Names = c(\"Min\", \"1Q\", \"Median\", \"3Q\", \"Max\")), 4L, TRUE, NULL, NULL, FALSE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

