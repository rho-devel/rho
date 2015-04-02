expected <- eval(parse(text="structure(list(quote(list), x = quote(c(2:3, NA)), y = quote(c(3:4, NA))), .Names = c(\"\", \"x\", \"y\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(x = c(2:3, NA), y = c(3:4, NA))), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

