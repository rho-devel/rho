expected <- eval(parse(text=".Primitive(\"+\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(.Primitive(\"+\"), TRUE)"));  
.Internal(print.function(argv[[1]], argv[[2]]));  
}, o=expected);  

