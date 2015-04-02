expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(1L, 3L, 2L, 4L), TRUE)"));        
.Internal(is.unsorted(argv[[1]], argv[[2]]));        
}, o=expected);        

