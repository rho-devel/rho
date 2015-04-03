expected <- eval(parse(text="structure(list(d = c(2, 1.73205080756888, 1.4142135623731, 1), u = structure(c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0), .Dim = c(4L, 4L)), vt = structure(c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0), .Dim = c(4L, 4L))), .Names = c(\"d\", \"u\", \"vt\"))"));                                                 
test(id=0, code={                                                 
argv <- eval(parse(text="list(\"S\", structure(c(1, 0, 0, 0, 0, 1.4142135623731, 0, 0, 0, 0, 1.73205080756888, 0, 0, 0, 0, 2), .Dim = c(4L, 4L), Dimnames = list(character(0), character(0))), c(2, 1.73205080756888, 1.4142135623731, 1), structure(c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0), .Dim = c(4L, 4L)), structure(c(0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0), .Dim = c(4L, 4L)))"));                                                 
.Internal(La_svd(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));                                                 
}, o=expected);                                                 

