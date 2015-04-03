expected <- eval(parse(text="list(structure(TRUE, .Dim = c(1L, 1L)))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(TRUE, .Dim = c(1L, 1L)))"));         
do.call(`list`, argv);         
}, o=expected);         

