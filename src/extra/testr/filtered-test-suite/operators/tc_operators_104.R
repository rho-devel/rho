expected <- eval(parse(text="structure(c(TRUE, FALSE, FALSE), .Dim = c(1L, 3L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(-1.47120249322699, 0.791288894530825, 4.43110323863505), .Dim = c(1L, 3L)), 0.0031308)"));             
do.call(`<=`, argv);             
}, o=expected);             

