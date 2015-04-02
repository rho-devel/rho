expected <- eval(parse(text="c(0, 0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"double\", 2)"));   
.Internal(`vector`(argv[[1]], argv[[2]]));   
}, o=expected);   

