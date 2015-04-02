expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(group = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c(\"Ctl\", \"Trt\"), class = \"factor\")), .Names = \"group\", class = \"data.frame\", row.names = c(NA, 20L)), \"any\")"));        
.Internal(`is.vector`(argv[[1]], argv[[2]]));        
}, o=expected);        

