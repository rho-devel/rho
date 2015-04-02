expected <- eval(parse(text="structure(c(0, 4, 10, 5, 2, 0, 4, 12, 9, 0, 0, 0, 12, 9, 2), .Dim = c(3L, 5L), .Dimnames = list(NULL, c(\"a\", \"b\", \"c\", \"d\", \"e\")))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(5, 2, 0, 2, 5, 2, 0, 2, 5), .Dim = c(3L, 3L)), structure(c(0, 1, 0, 0, 2, 0, 0, 2, 0, 1, 2, 0, 1, 0, 0), .Dim = c(5L, 3L), .Dimnames = list(c(\"a\", \"b\", \"c\", \"d\", \"e\"), NULL)))"));       
.Internal(tcrossprod(argv[[1]], argv[[2]]));       
}, o=expected);       

