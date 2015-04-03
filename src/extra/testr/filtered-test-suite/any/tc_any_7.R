expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = 3:4, .Dimnames = structure(list(x1 = c(\"a\", \"b\", \"c\"), x2 = c(\"a\", \"b\", \"c\", NA)), .Names = c(\"x1\", \"x2\"))))"));             
do.call(`any`, argv);             
}, o=expected);             

