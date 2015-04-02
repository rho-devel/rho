expected <- eval(parse(text="integer(0)"));            
test(id=0, code={            
argv <- eval(parse(text="list(0L, 0L, FALSE, NULL)"));            
.Internal(sample(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));            
}, o=expected);            

