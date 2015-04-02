expected <- eval(parse(text="structure(list(quote(list), ii = quote(1:10), xx = quote(pi * -3:6)), .Names = c(\"\", \"ii\", \"xx\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(ii = 1:10, xx = pi * -3:6)), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

