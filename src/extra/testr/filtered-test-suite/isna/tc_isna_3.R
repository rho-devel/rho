expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE), .Dim = 4L, .Dimnames = structure(list(N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = \"N\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(17L, 18L, 18L, 18L), .Dim = 4L, .Dimnames = structure(list(N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = \"N\")))"));        
do.call(`is.na`, argv);        
}, o=expected);        

