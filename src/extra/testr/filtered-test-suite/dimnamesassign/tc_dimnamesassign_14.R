expected <- eval(parse(text="structure(c(0, 0, 0, 0), .Dim = c(2L, 2L), .Dimnames = list(NULL, c(\"A\", \"B\")))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0), .Dim = c(2L, 2L), .Dimnames = list(NULL, c(\"A\", \"B\"))), value = list(NULL, c(\"A\", \"B\")))"));        
do.call(`dimnames<-`, argv);        
}, o=expected);        

