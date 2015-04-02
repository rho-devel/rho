expected <- eval(parse(text="1:54"));            
test(id=0, code={            
argv <- eval(parse(text="list(1:54, FALSE)"));            
.Internal(sort(argv[[1]], argv[[2]]));            
}, o=expected);            

