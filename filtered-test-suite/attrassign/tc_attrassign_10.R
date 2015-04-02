expected <- eval(parse(text="structure(list(a = 1:3, b = structure(1:3, .Label = c(\"a\", \"b\", \"c\"), class = \"factor\")), .Names = c(\"a\", \"b\"), row.names = c(NA, -3L), class = \"data.frame\", foo = 10)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(a = 1:3, b = structure(1:3, .Label = c(\"a\", \"b\", \"c\"), class = \"factor\")), .Names = c(\"a\", \"b\"), row.names = c(NA, -3L), class = \"data.frame\", foo = 10), \"foo\", value = 10)"));  
do.call(`attr<-`, argv);  
}, o=expected);  

