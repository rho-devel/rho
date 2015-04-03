expected <- eval(parse(text="structure(c(1L, 5L, 9L, 13L, 17L, 21L, 2L, 6L, 10L, 14L, 18L, 22L, 3L, 7L, 11L, 15L, 19L, 23L, 4L, 8L, 12L, 16L, 20L, 24L), .Dim = c(6L, 4L), .Dimnames = structure(list(sad = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\"), happy = c(\"a\", \"b\", \"c\", \"d\")), .Names = c(\"sad\", \"happy\")))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:24, .Dim = c(4L, 6L), .Dimnames = structure(list(happy = c(\"a\", \"b\", \"c\", \"d\"), sad = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\")), .Names = c(\"happy\", \"sad\"))), c(2, 1), TRUE)"));   
.Internal(`aperm`(argv[[1]], argv[[2]], argv[[3]]));   
}, o=expected);   

