expected <- eval(parse(text="list(quote(list), quote(X[[2L]]))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(X[[2L]])), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

