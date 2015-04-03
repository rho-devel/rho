expected <- eval(parse(text="list(quote(list), quote(y), quote(x1), quote(x2))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(y, x1, x2)), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

