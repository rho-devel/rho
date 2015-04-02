expected <- eval(parse(text="structure(c(1, 0, -1, 0.5, -0.5, NA, NA, NA, 0), .Dim = c(3L, 3L))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(1, 0, -1, 0.5, -0.5, NA, NA, NA, 0), .Dim = c(3L, 3L)), 1:2, TRUE)"));   
.Internal(`aperm`(argv[[1]], argv[[2]], argv[[3]]));   
}, o=expected);   

