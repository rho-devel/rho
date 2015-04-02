expected <- eval(parse(text="c(NA, NA, 1L)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"1\", \"2\", NA), NA_real_, NA_integer_, NULL)"));        
.Internal(`match`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

