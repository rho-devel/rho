expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1, 2, 3, 0, 10, NA), .Dim = c(3L, 2L)))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

