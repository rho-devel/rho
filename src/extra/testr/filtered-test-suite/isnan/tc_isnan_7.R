expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE), .Dim = c(3L, 1L))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:3, .Dim = c(3L, 1L)))"));   
do.call(`is.nan`, argv);   
}, o=expected);   

