expected <- eval(parse(text="3"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(1, 0, 0, 0, 1, 0, 0, 0, 1), .Dim = c(3L, 3L)), extrarg = FALSE)"));                 
do.call(`sum`, argv);                 
}, o=expected);                 

