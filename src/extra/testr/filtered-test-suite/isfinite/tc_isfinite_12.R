expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(0.0599, 0.00599, 0.000599, 5.99e-05, 5.99e-06, 5.99e-07))"));      
do.call(`is.finite`, argv);      
}, o=expected);      

