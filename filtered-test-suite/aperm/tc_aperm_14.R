expected <- eval(parse(text="structure(character(0), .Dim = c(3L, 0L, 2L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(character(0), .Dim = c(3L, 0L, 2L)), 1:3, TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

