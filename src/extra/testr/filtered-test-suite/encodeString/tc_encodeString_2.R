expected <- eval(parse(text="\"\\\" \\\\t\\\\n\\\\\\\"\\\\\\\\'`><=%;,|&{()}\\\"\""));        
test(id=0, code={        
argv <- eval(parse(text="list(\" \\t\\n\\\"\\\\'`><=%;,|&{()}\", 0L, \"\\\"\", 0L, FALSE)"));        
.Internal(encodeString(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));        
}, o=expected);        

