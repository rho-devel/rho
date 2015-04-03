expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(a = 1), .Dim = 1L, .Dimnames = list(\"a\")))"));              
do.call(`is.call`, argv);              
}, o=expected);              

