expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(-32.6915744137254, -24.6945405669482, -24.6945405669482, -357.79068676373), .Dim = c(2L, 2L)))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

