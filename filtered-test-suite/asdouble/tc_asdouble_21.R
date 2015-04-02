expected <- eval(parse(text="c(5, 2)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(foo = 5L, Species = 2L), .Names = c(\"foo\", \"Species\"), out.attrs = structure(list(dim = structure(c(6L, 3L), .Names = c(\"foo\", \"Species\")), dimnames = structure(list(foo = c(\"foo=1\", \"foo=2\", \"foo=3\", \"foo=4\", \"foo=5\", \"foo=6\"), Species = c(\"Species=1\", \"Species=2\", \"Species=3\")), .Names = c(\"foo\", \"Species\"))), .Names = c(\"dim\", \"dimnames\")), row.names = 11L, class = \"data.frame\"))"));               
do.call(`as.double`, argv);               
}, o=expected);               

