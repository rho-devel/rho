expected <- eval(parse(text="list(\"a\", \"b\", \"c\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(\"a\", \"b\", \"c\"), \"pairlist\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

