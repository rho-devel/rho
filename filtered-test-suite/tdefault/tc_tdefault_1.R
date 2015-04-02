expected <- eval(parse(text="structure(c(\"D:\", \"E:\", \"F:\", \"G:\"), .Dim = c(1L, 4L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(\"D:\", \"E:\", \"F:\", \"G:\"), .Dim = c(4L, 1L)))"));  
.Internal(`t.default`(argv[[1]]));  
}, o=expected);  

