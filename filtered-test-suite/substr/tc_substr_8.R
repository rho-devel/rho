expected <- eval(parse(text="structure(c(\"model.\", \"predic\", \"residu\"), .Names = c(\"model.frame\", \"predict\", \"residuals\"))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(\"model.frame\", \"predict\", \"residuals\"), .Names = c(\"model.frame\", \"predict\", \"residuals\")), 1L, 6L)"));                
.Internal(substr(argv[[1]], argv[[2]], argv[[3]]));                
}, o=expected);                

