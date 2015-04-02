expected <- eval(parse(text="structure(list(`1` = c(1L, 3L, 5L), `2` = c(2L, 4L, 6L)), .Names = c(\"1\", \"2\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(1:6, structure(1:2, .Label = c(\"1\", \"2\"), class = \"factor\"))"));  
.Internal(`split`(argv[[1]], argv[[2]]));  
}, o=expected);  

