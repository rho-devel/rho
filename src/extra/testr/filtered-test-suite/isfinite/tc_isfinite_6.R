expected <- eval(parse(text="structure(TRUE, .Dim = c(1L, 1L), .Dimnames = list(\"(Intercept)\", \"(Intercept)\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(1.27861470300044, .Dim = c(1L, 1L), .Dimnames = list(\"(Intercept)\", \"(Intercept)\")))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

