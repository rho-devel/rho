expected <- eval(parse(text="integer(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(logical(0))"));   
.Internal(`which`(argv[[1]]));   
}, o=expected);   

