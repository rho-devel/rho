expected <- eval(parse(text="structure(list(A = NULL, B = NULL, `NA` = NULL), .Names = c(\"A\", \"B\", NA), row.names = c(NA, -11L), class = \"data.frame\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(list(A = 0:10, B = 10:20, `NA` = 20:30), .Names = c(\"A\", \"B\", NA), row.names = c(NA, -11L), class = \"data.frame\"), structure(list(A = NULL, B = NULL, `NA` = NULL), .Names = c(\"A\", \"B\", NA), row.names = c(NA, -11L), class = \"data.frame\"))"));    
.Internal(`copyDFattr`(argv[[1]], argv[[2]]));    
}, o=expected);    

