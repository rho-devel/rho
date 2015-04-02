expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(2L, 1L, 0L, 3L), FALSE)"));        
.Internal(is.unsorted(argv[[1]], argv[[2]]));        
}, o=expected);        

