expected <- eval(parse(text="c(\"\\\"gray17\\\"\", \"\\\"grey17\\\"\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"\\\"gray17\\\"\", \"\\\"grey17\\\"\"), 128)"));        
.Internal(strtrim(argv[[1]], argv[[2]]));        
}, o=expected);        

