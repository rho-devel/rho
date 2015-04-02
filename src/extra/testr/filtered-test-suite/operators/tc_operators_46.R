expected <- eval(parse(text="structure(c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE), .Dim = c(3L, 2L))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(14, 2, 0, 2, -7, 0), .Dim = c(3L, 2L)), 0)"));         
do.call(`!=`, argv);         
}, o=expected);         

