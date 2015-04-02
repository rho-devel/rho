expected <- eval(parse(text="structure(c(2L, 4L, 5L, 10L, 8L, 16L, 11L, 22L), .Dim = c(2L, 4L), .Dimnames = list(c(\"X\", \"Y\"), NULL))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1:12, .Dim = 3:4), c(\"Y\", \"X\", \"Y\"), c(\"X\", \"Y\"), FALSE, c(\"X\", \"Y\"))"));  
.Internal(`rowsum_matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));  
}, o=expected);  

