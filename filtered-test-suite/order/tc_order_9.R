expected <- eval(parse(text="integer(0)"));             
test(id=0, code={             
argv <- eval(parse(text="list(TRUE, FALSE, structure(numeric(0), .Dim = c(0L, 0L), .Dimnames = list(NULL, NULL)))"));             
.Internal(order(argv[[1]], argv[[2]], argv[[3]]));             
}, o=expected);             

