expected <- eval(parse(text=".Primitive(\"c\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(.Primitive(\"c\"), TRUE)"));  
.Internal(print.function(argv[[1]], argv[[2]]));  
}, o=expected);  

