expected <- eval(parse(text="structure(list(`1` = NA, `2` = NA), .Names = c(\"1\", \"2\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(NA, NA), .Dim = 1:2), structure(1:2, .Label = c(\"1\", \"2\"), class = \"factor\"))"));  
.Internal(`split`(argv[[1]], argv[[2]]));  
}, o=expected);  

