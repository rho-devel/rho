expected <- eval(parse(text=".Primitive(\".Internal\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(.Primitive(\".Internal\"), TRUE)"));  
.Internal(print.function(argv[[1]], argv[[2]]));  
}, o=expected);  

