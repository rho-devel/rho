expected <- eval(parse(text="integer(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 0L)))"));  
.Internal(bitwiseNot(argv[[1]]));  
}, o=expected);  

