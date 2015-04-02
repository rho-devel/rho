expected <- eval(parse(text="structure(c(3, 4, 3, 3, 3, 2, 3, 1, 3, 2, 3, 3, 3, 4, 3, 5), .Dim = c(2L, 8L), .Dimnames = list(c(\"x1\", \"x2\"), c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\")))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 2, 1, 2, 3, 4, 5), .Dim = c(8L, 2L), .Dimnames = list(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\"), c(\"x1\", \"x2\"))), c(2L, 1L), TRUE)"));   
.Internal(`aperm`(argv[[1]], argv[[2]], argv[[3]]));   
}, o=expected);   

