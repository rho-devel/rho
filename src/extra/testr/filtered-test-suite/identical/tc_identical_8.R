expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(a = 1), .Names = \"a\", .Tsp = c(1, 1, 1), class = \"ts\"), structure(list(a = 1), .Names = \"a\", .Tsp = c(1, 1, 1), class = \"ts\"), TRUE, TRUE, TRUE, TRUE, FALSE)"));             
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

