expected <- eval(parse(text="2.22044604925031e-16"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(-2.16610675289233, 2.16610675289233))"));   
.Internal(`mean`(argv[[1]]));   
}, o=expected);   

