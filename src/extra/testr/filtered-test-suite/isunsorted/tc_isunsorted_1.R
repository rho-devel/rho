expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(1L, 2L, 4L), FALSE)"));        
.Internal(is.unsorted(argv[[1]], argv[[2]]));        
}, o=expected);        

