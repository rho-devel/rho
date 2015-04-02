expected <- eval(parse(text="structure(c(\"4\", \"5\", \"6\", \"7\", \"8\", \"9\"), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"a\", \"b\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(4:9, .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"a\", \"b\"))), FALSE, NULL, 0L, NULL, 3L, FALSE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

