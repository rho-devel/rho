expected <- eval(parse(text="structure(list(quote(list), x = quote(1:100), z = quote(1:100 + rnorm(100, 10))), .Names = c(\"\", \"x\", \"z\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(x = 1:100, z = 1:100 + rnorm(100, 10))), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

