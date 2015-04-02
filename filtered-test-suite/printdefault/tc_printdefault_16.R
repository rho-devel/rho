expected <- eval(parse(text="structure(c(\"1\", \"2\", \"3\", \"\\abc\"), .Dim = c(1L, 4L), .Dimnames = list(\"1\", c(\"V1\", \"V2\", \"V3\", \"V4\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"1\", \"2\", \"3\", \"\\abc\"), .Dim = c(1L, 4L), .Dimnames = list(\"1\", c(\"V1\", \"V2\", \"V3\", \"V4\"))), NULL, FALSE, NULL, NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

