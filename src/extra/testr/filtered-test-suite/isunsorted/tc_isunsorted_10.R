expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(8, 6, 9, 4, 3, 7, 1, 5, 2), TRUE)"));        
.Internal(is.unsorted(argv[[1]], argv[[2]]));        
}, o=expected);        

