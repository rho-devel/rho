expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = 1:3)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1), .Dim = 1:3))"));   
do.call(`is.nan`, argv);   
}, o=expected);   

