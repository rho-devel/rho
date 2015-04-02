expected <- eval(parse(text="1:6"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(1, 2, 3, 4, 8, 12), c(1, 2, 3, 4, 8, 12), NA_integer_, NULL)"));        
.Internal(`match`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

