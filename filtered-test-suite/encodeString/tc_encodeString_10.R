expected <- eval(parse(text="structure(character(0), .Dim = c(0L, 0L))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(character(0), .Dim = c(0L, 0L)), 0L, \"\", 0L, TRUE)"));        
.Internal(encodeString(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));        
}, o=expected);        

