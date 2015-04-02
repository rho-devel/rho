expected <- eval(parse(text="c(\"Inf\", \"-Inf\", \"NaN\", NA)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(Inf, -Inf, NaN, NA))"));        
do.call(`as.character`, argv);        
}, o=expected);        

