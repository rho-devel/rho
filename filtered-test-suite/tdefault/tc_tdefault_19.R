expected <- eval(parse(text="structure(c(3, 4), .Dim = 1:2)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(3, 4))"));  
.Internal(`t.default`(argv[[1]]));  
}, o=expected);  

