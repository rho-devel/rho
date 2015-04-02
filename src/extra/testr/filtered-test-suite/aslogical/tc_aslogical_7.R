expected <- eval(parse(text="logical(0)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(logical(0), .Dim = c(0L, 0L), .Dimnames = list(NULL, NULL)))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

