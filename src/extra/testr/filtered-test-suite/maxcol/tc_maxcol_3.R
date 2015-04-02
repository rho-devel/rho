expected <- eval(parse(text="integer(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(), .Dim = 0:1), 1L)"));  
.Internal(max.col(argv[[1]], argv[[2]]));  
}, o=expected);  

