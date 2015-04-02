expected <- eval(parse(text="structure(c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE), .Dim = c(7L, 7L), .Dimnames = list(NULL, NULL))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(21, 0, 77, 0, 0, 96, 0, 0, 0, 0, 0, 0, 0, 0, 77, 0, 325, 0, 0, 288, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 361, 0, 513, 96, 0, 288, 0, 0, 576, 0, 0, 0, 0, 0, 513, 0, 729), .Dim = c(7L, 7L), .Dimnames = list(NULL, NULL)), 0)"));         
do.call(`!=`, argv);         
}, o=expected);         

