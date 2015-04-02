expected <- eval(parse(text="structure(1, .Dim = 1L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1, .Dim = 1L), structure(1L, class = c(\"terminal\", \"connection\")), 95)"));  
.Internal(dput(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

