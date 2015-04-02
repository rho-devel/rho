expected <- eval(parse(text="c(\"    a\", \"   ab\", \"abcde\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"a\", \"ab\", \"abcde\"), NA, \"\", 1L, TRUE)"));        
.Internal(encodeString(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));        
}, o=expected);        

