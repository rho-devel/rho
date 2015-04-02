expected <- eval(parse(text="structure(c(\"abc\", \"def\\\"gh\"), .Dim = 1:2, .Dimnames = list(\"1\", c(\"a\", \"b\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"abc\", \"def\\\"gh\"), .Dim = 1:2, .Dimnames = list(\"1\", c(\"a\", \"b\"))), NULL, FALSE, NULL, NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

