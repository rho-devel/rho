expected <- eval(parse(text="c(1, 2, 3, 2, 2, 2, 2)"));           
test(id=0, code={           
argv <- eval(parse(text="list(FALSE, structure(c(2, 3, 4, 2, 2, 2), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"a\", \"\"))), 1:7)"));           
.Internal(pmin(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

