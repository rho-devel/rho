expected <- eval(parse(text="FALSE"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(12L, 120L, 116L), .Dim = 3L, .Dimnames = structure(list(c(\"0-5yrs\", \"6-11yrs\", \"12+ yrs\")), .Names = \"\"), class = \"table\"), \"any\")"));        
.Internal(`is.vector`(argv[[1]], argv[[2]]));        
}, o=expected);        

