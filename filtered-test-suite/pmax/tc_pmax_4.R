expected <- eval(parse(text="numeric(0)"));            
test(id=0, code={            
argv <- eval(parse(text="list(FALSE, 0, numeric(0))"));            
.Internal(pmax(argv[[1]], argv[[2]], argv[[3]]));            
}, o=expected);            

