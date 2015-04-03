expected <- eval(parse(text="NaN"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(-Inf, Inf))"));   
.Internal(`mean`(argv[[1]]));   
}, o=expected);   

