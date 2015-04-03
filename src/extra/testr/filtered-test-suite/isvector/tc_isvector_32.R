expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1L, 3L, 2L, 3L, 3L, 1L, 2L, 3L, 2L, 2L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\"), \"symbol\")"));        
.Internal(`is.vector`(argv[[1]], argv[[2]]));        
}, o=expected);        

