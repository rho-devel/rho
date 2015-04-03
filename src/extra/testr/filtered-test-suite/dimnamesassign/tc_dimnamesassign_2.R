expected <- eval(parse(text="structure(1:24, .Dim = 2:4, .Dimnames = list(c(\"A\", \"B\"), NULL, NULL))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(1:24, .Dim = 2:4, .Dimnames = list(c(\"A\", \"B\"), NULL, NULL)), value = list(c(\"A\", \"B\"), NULL, NULL))"));        
do.call(`dimnames<-`, argv);        
}, o=expected);        

