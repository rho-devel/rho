expected <- eval(parse(text=".Primitive(\"if\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(.Primitive(\"if\"), TRUE)"));  
.Internal(print.function(argv[[1]], argv[[2]]));  
}, o=expected);  

