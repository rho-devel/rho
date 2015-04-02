expected <- eval(parse(text="c(2L, 1L, 3L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(2L, 1L, 3L), .Label = c(\"1\", \"2\", NA), class = \"factor\"), TRUE, FALSE)"));  
.Internal(`radixsort`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

