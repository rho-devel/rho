expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = 3:4, .Dimnames = structure(list(x1 = c(\"a\", \"b\", \"c\"), x2 = c(\"a\", \"b\", \"c\", NA)), .Names = c(\"x1\", \"x2\")))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(2, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0), .Dim = 3:4, .Dimnames = structure(list(x1 = c(\"a\", \"b\", \"c\"), x2 = c(\"a\", \"b\", \"c\", NA)), .Names = c(\"x1\", \"x2\")), class = c(\"xtabs\", \"table\")))"));                
do.call(`is.na`, argv);                
}, o=expected);                

