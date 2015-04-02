expected <- eval(parse(text="structure(c(0, 0, 0, 0, 0.693147180559945, 0, 0, 0, 1.79175946922805), .Dim = c(3L, 3L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(2, 1, 1, 1, 3, 1, 1, 1, 4), .Dim = c(3L, 3L)))"));  
do.call(`lgamma`, argv);  
}, o=expected);  

