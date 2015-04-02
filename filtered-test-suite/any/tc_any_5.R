expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(14, 2, 0, 2, -7, 0), .Dim = c(3L, 2L)))"));             
do.call(`any`, argv);             
}, o=expected);             

