expected <- eval(parse(text="structure(\"\\\"@CRAN@\\\"\", .Names = \"CRAN\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(\"\\\"@CRAN@\\\"\", .Names = \"CRAN\"), 128)"));        
.Internal(strtrim(argv[[1]], argv[[2]]));        
}, o=expected);        

