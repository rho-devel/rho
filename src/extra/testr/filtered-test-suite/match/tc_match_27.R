expected <- eval(parse(text="c(0L, 0L, 0L, 0L, 0L)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"2005-01-01\", \"2006-01-01\", \"2007-01-01\", \"2008-01-01\", \"2009-01-01\"), c(NA, NaN), 0L, NULL)"));        
.Internal(`match`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

