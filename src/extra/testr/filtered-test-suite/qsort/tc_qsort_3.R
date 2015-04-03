expected <- eval(parse(text="numeric(0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(numeric(0), FALSE)"));        
.Internal(qsort(argv[[1]], argv[[2]]));        
}, o=expected);        

