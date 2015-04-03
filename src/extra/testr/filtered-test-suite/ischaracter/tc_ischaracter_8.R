expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(238L, 154L, 73L), .Dim = c(3L, 1L), .Dimnames = list(c(\"red\", \"green\", \"blue\"), NULL)))"));         
do.call(`is.character`, argv);         
}, o=expected);         

