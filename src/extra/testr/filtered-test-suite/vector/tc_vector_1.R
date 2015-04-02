expected <- eval(parse(text="integer(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"integer\", 0L)"));   
.Internal(`vector`(argv[[1]], argv[[2]]));   
}, o=expected);   

