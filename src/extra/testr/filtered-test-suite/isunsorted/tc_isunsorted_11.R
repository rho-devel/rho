expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(1, 2, 3, 2), FALSE)"));        
.Internal(is.unsorted(argv[[1]], argv[[2]]));        
}, o=expected);        

