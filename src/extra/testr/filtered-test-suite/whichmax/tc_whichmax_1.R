expected <- eval(parse(text="1L"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = 4:5, .Dimnames = list(c(\"I(a)\", \"b\", \"c\", \"a\"), c(\"I(a)\", \"b\", \"c\", \"b:c\", \"a:x\"))))"));    
.Internal(which.max(argv[[1]]));    
}, o=expected);    

