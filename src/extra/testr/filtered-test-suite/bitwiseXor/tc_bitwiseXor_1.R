expected <- eval(parse(text="-2L"));  
test(id=0, code={  
argv <- eval(parse(text="list(-1L, 1L)"));  
.Internal(bitwiseXor(argv[[1]], argv[[2]]));  
}, o=expected);  

