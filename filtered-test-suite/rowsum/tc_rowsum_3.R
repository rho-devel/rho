expected <- eval(parse(text="structure(numeric(0), .Dim = 0:1, .Dimnames = list(character(0), NULL))"));       
test(id=0, code={       
argv <- eval(parse(text="list(numeric(0), numeric(0), numeric(0), FALSE, character(0))"));       
.Internal(rowsum_matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));       
}, o=expected);       

