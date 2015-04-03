expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(1+2i, 5+0i, 3-4i, -6+0i), .Dim = c(2L, 2L)))"));              
do.call(`is.expression`, argv);              
}, o=expected);              

