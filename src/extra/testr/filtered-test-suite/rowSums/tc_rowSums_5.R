expected <- eval(parse(text="numeric(0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 3L), .Dimnames = list(NULL, c(\"wt.loss\", \"age\", \"I(age)\"))), 0, 3, FALSE)"));        
.Internal(rowSums(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

