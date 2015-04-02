expected <- eval(parse(text="character(0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(character(0), 40L)"));        
.Internal(strtrim(argv[[1]], argv[[2]]));        
}, o=expected);        

