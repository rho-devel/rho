expected <- eval(parse(text="TRUE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(25L, 0L)))"));                 
do.call(`is.matrix`, argv);                 
}, o=expected);                 

