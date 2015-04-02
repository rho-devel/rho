expected <- eval(parse(text="structure(c(3, 3, NA, 3, 3, 3, 3, 3, 4, 3, NA, NA, 2, 3, 4, 5), .Dim = c(8L, 2L), .Dimnames = list(NULL, c(\"x1\", \"x2\")))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(3+2i, 3+2i, NA, 3+2i, 3+2i, 3+2i, 3+2i, 3+2i, 4-5i, 3-5i, NA, NA, 2-5i, 3-5i, 4-5i, 5-5i), .Dim = c(8L, 2L), .Dimnames = list(NULL, c(\"x1\", \"x2\"))))"));         
do.call(`Re`, argv);         
}, o=expected);         

