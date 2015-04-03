expected <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = 2:3, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"))), structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L, 5L)), TRUE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = 2:3, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"))), structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L, 5L)), TRUE)"));         
do.call(`list`, argv);         
}, o=expected);         

