expected <- eval(parse(text="structure(c(NA, NA, NA, \"a\", NA, NA, \"b\", \"d\", NA, \"10\", \"12\", \"14\"), .Dim = 3:4)"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(NA, NA, NA, \"a\", NA, NA, \"b\", \"d\", NA, \"10\", \"12\", \"14\"), .Dim = 3:4), NULL, TRUE, \"----\", NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

