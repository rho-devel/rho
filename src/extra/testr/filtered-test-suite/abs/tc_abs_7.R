expected <- eval(parse(text="structure(c(3.56092495589032e-15, 3.3833492686504e-16, 6.89814774385614e-17, 1.77454768520688e-17), .Dim = c(2L, 2L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(3.5527136788005e-15+2.4168586625265e-16i, 2.4980018054066e-16-2.28189378671807e-16i, 0-6.89814774385614e-17i, 0-1.77454768520688e-17i), .Dim = c(2L, 2L)))"));              
do.call(`abs`, argv);              
}, o=expected);              

