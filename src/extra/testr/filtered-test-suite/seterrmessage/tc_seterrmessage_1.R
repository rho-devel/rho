expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"Error in cor(rnorm(10), NULL) : \\n  supply both 'x' and 'y' or a matrix-like 'x'\\n\")"));  
.Internal(`seterrmessage`(argv[[1]]));  
}, o=expected);  

