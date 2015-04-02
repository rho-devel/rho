expected <- eval(parse(text=".Primitive(\"log\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(.Primitive(\"log\"), TRUE)"));  
.Internal(print.function(argv[[1]], argv[[2]]));  
}, o=expected);  

