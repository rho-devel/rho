expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(TRUE, 1L)"));            
.Internal(rep.int(argv[[1]], argv[[2]]));            
}, o=expected);            

