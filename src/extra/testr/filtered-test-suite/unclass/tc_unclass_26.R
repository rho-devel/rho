expected <- eval(parse(text="structure(list(a = 1), .Dim = 1L, .Dimnames = list(\"a\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(a = 1), .Dim = 1L, .Dimnames = list(\"a\")))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

