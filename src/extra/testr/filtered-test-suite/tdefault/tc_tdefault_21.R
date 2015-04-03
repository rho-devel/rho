expected <- eval(parse(text="structure(-3:5, .Dim = c(1L, 9L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(-3:5)"));  
.Internal(`t.default`(argv[[1]]));  
}, o=expected);  

