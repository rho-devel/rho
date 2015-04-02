expected <- eval(parse(text="5.5"));   
test(id=0, code={   
argv <- eval(parse(text="list(1:10)"));   
.Internal(`mean`(argv[[1]]));   
}, o=expected);   

