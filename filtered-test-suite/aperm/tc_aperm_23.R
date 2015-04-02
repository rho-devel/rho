expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE), .Dim = c(1L, 3L))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE), .Dim = c(3L, 1L)), c(2L, 1L), TRUE)"));   
.Internal(`aperm`(argv[[1]], argv[[2]], argv[[3]]));   
}, o=expected);   

