expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE), .Dim = c(3L, 3L))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), .Dim = c(3L, 3L)))"));          
do.call(`!`, argv);          
}, o=expected);          

