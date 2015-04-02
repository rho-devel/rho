expected <- eval(parse(text="structure(c(56.8666666666667, 52.8833333333333), .Dim = 2L, .Dimnames = structure(list(K = c(\"0\", \"1\")), .Names = \"K\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(56.8666666666667, 52.8833333333333), .Dim = 2L, .Dimnames = structure(list(K = c(\"0\", \"1\")), .Names = \"K\")))"));              
do.call(`abs`, argv);              
}, o=expected);              

