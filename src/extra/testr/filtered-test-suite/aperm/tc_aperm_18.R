expected <- eval(parse(text="structure(c(NA, NA, NA), .Dim = 3L)"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(NA, NA, NA), .Dim = 3L), 1L, TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

