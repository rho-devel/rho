expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L, 5L))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:20, .Dim = c(2L, 2L, 5L)), structure(1:20, .Dim = c(2L, 2L, 5L)))"));   
do.call(`==`, argv);   
}, o=expected);   

