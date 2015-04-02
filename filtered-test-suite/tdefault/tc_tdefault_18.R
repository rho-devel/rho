expected <- eval(parse(text="structure(c(0, 0, 0, 1954.88214285714, 0, 0, 557.144827586207, 1392.34285714286, 0), .Dim = c(3L, 3L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(0, 1954.88214285714, 557.144827586207, 0, 0, 1392.34285714286, 0, 0, 0), .Dim = c(3L, 3L)))"));  
.Internal(`t.default`(argv[[1]]));  
}, o=expected);  

