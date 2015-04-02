expected <- eval(parse(text="\"\\\"\\\\\\\"class\\\\\\\" is a reserved slot name and cannot be redefined\\\"\""));        
test(id=0, code={        
argv <- eval(parse(text="list(\"\\\"class\\\" is a reserved slot name and cannot be redefined\", 0L, \"\\\"\", 0L, FALSE)"));        
.Internal(encodeString(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));        
}, o=expected);        

