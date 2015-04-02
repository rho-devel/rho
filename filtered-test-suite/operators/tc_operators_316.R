expected <- eval(parse(text="structure(c(-1e-05, 1e-05, -1e-04, 1e-04, -0.001, 0.001, -0.01, 0.01, -0.1, 0.1, -1, 1, -10, 10, -100, 100, -1000, 1000, -10000, 10000, -1e+05, 1e+05), .Dim = c(2L, 11L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(-1, 1), structure(c(1e-05, 1e-04, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 1e+05), .Dim = c(1L, 11L)))"));              
do.call(`%*%`, argv);              
}, o=expected);              

