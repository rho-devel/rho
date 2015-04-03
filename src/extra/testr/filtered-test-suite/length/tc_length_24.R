expected <- eval(parse(text="1L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(a = 1), .Dim = 1L, .Dimnames = list(\"a\")))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

