expected <- eval(parse(text="3"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1:5, .Dim = c(5L, 1L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\"), \"a\")), 5, 1, FALSE)"));  
.Internal(`colMeans`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

