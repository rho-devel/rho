expected <- eval(parse(text="structure(c(TRUE, FALSE, TRUE, TRUE), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\")))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(2, 0, 1, 2), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\"))), value = \"logical\")"));  
do.call(`storage.mode<-`, argv);  
}, o=expected);  

