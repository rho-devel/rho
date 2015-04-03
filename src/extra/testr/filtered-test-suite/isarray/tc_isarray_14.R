expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(a = 1), .Dim = 1L, .Dimnames = list(\"a\")))"));            
do.call(`is.array`, argv);            
}, o=expected);            

