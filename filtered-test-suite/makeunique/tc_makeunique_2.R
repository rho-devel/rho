expected <- eval(parse(text="c(\"b\", \"NA\", \"NA.1\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(\"b\", \"NA\", \"NA\"), \".\")"));   
.Internal(`make.unique`(argv[[1]], argv[[2]]));   
}, o=expected);   

