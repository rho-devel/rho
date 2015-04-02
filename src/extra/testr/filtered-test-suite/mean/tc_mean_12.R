expected <- eval(parse(text="0.75"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(TRUE, FALSE, TRUE, TRUE))"));   
.Internal(`mean`(argv[[1]]));   
}, o=expected);   

