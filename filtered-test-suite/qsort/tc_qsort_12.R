expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(FALSE, FALSE)"));        
.Internal(qsort(argv[[1]], argv[[2]]));        
}, o=expected);        

