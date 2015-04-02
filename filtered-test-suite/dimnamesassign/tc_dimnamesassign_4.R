expected <- eval(parse(text="structure(numeric(0), .Dim = c(0L, 20L))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 20L)), value = NULL)"));        
do.call(`dimnames<-`, argv);        
}, o=expected);        

