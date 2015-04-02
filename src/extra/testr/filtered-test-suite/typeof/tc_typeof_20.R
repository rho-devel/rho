expected <- eval(parse(text="\"list\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(a = 1), .Dim = 1L, .Dimnames = list(\"a\")))"));                
.Internal(typeof(argv[[1]]));                
}, o=expected);                

