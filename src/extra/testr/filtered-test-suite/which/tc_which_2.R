expected <- eval(parse(text="1:4"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\"))))"));            
.Internal(which(argv[[1]]));            
}, o=expected);            

