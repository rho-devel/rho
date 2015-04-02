expected <- eval(parse(text="structure(1:9, .Dim = c(3L, 3L), .Dimnames = list(c(\"x\", \"y\", NA), c(\"1\", NA, \"3\")))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(1:9, .Dim = c(3L, 3L), .Dimnames = list(c(\"x\", \"y\", NA), c(\"1\", NA, \"3\"))), value = list(c(\"x\", \"y\", NA), c(\"1\", NA, \"3\")))"));  
do.call(`dimnames<-`, argv);  
}, o=expected);  

