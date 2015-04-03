expected <- eval(parse(text="c(63, 64, 187, 188)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(63, 187, 64, 188), FALSE)"));        
.Internal(qsort(argv[[1]], argv[[2]]));        
}, o=expected);        

