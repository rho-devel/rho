expected <- eval(parse(text="c(\"2\", \"1\", NA)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(2L, 1L, NA))"));        
do.call(`as.character`, argv);        
}, o=expected);        

