expected <- eval(parse(text="c(2e-08, 2e-08, 2e-08, 2e-08, 2e-08, 2e-08, 2e-08, 2e-08, 2e-08)"));            
test(id=0, code={            
argv <- eval(parse(text="list(2e-08, 9)"));            
.Internal(rep.int(argv[[1]], argv[[2]]));            
}, o=expected);            

