expected <- eval(parse(text="c(0.8625, 0.8625)"));            
test(id=0, code={            
argv <- eval(parse(text="list(0.8625, 2)"));            
.Internal(rep.int(argv[[1]], argv[[2]]));            
}, o=expected);            

