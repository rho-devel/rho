expected <- eval(parse(text="structure(c(2, 2, NA, 2, 2, 2, 2, 2, -5, -5, NA, NA, -5, -5, -5, -5), .Dim = c(8L, 2L), .Dimnames = list(NULL, c(\"x1\", \"x2\")))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(3+2i, 3+2i, NA, 3+2i, 3+2i, 3+2i, 3+2i, 3+2i, 4-5i, 3-5i, NA, NA, 2-5i, 3-5i, 4-5i, 5-5i), .Dim = c(8L, 2L), .Dimnames = list(NULL, c(\"x1\", \"x2\"))))"));         
do.call(`Im`, argv);         
}, o=expected);         

