expected <- eval(parse(text="c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\"), \".\")"));   
.Internal(`make.unique`(argv[[1]], argv[[2]]));   
}, o=expected);   

