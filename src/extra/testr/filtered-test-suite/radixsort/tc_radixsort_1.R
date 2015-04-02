expected <- eval(parse(text="1L"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1L, .Label = c(\"Ctl\", \"Trt\"), class = \"factor\"), TRUE, FALSE)"));  
.Internal(`radixsort`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

