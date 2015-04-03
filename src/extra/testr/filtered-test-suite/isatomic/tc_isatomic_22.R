expected <- eval(parse(text="TRUE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(4L, 0L)))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

