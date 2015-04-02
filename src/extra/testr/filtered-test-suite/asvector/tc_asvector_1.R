expected <- eval(parse(text="list(\"ylog\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(\"ylog\", \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

