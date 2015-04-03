expected <- eval(parse(text="structure(c(\"1\", \"2\", \"\\\\b\", \"4\", \"5\", \"\\\\040\", \"\\\\x20\", \"c:\\\\spencer\\\\tests\", \"\\\\t\", \"\\\\n\", \"\\\\r\"), .Dim = c(11L, 1L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\", \"11\"), \"TEST\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"1\", \"2\", \"\\\\b\", \"4\", \"5\", \"\\\\040\", \"\\\\x20\", \"c:\\\\spencer\\\\tests\", \"\\\\t\", \"\\\\n\", \"\\\\r\"), .Dim = c(11L, 1L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\", \"11\"), \"TEST\")), NULL, FALSE, NULL, NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

