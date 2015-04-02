expected <- eval(parse(text="structure(list(quote(list), a = quote(1:3), b = quote(letters[1:3])), .Names = c(\"\", \"a\", \"b\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(a = 1:3, b = letters[1:3])), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

