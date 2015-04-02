expected <- eval(parse(text="structure(c(\"1\", \"2\", \"1\"), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"1\", \"2\", \"1\"), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\")), NULL, FALSE, NULL, NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

