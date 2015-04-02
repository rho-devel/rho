expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(1, 0, 0, 0, NA, 6, 0, 0, 0, 14, 3, 0, 15, 0, 0, 8), .Dim = c(4L, 4L)))"));         
do.call(`is.logical`, argv);         
}, o=expected);         

