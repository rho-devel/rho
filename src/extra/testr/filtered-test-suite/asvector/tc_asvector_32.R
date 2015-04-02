expected <- eval(parse(text="structure(list(quote(list), a = quote(I(\"abc\")), b = quote(I(\"def\\\"gh\"))), .Names = c(\"\", \"a\", \"b\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(a = I(\"abc\"), b = I(\"def\\\"gh\"))), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

