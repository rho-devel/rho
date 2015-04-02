expected <- eval(parse(text="structure(list(3, 3, 3, 3, 3, \"fred\"), .Dim = c(3L, 2L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(list(3, 3, 3, 3, 3, \"fred\"), .Dim = 2:3), NULL, TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

