expected <- eval(parse(text="quote(diff)"));       
test(id=0, code={       
argv <- eval(parse(text="list(\"diff\", \"symbol\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

