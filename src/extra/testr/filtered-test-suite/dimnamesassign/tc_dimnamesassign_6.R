expected <- eval(parse(text="structure(c(-75, 0, 103, 0, 124, -1, 0, -2.77555756156289e-17, 0, -1.66533453693773e-16, 0, 0, 0, 178, 0), .Dim = c(5L, 3L))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(-75, 0, 103, 0, 124, -1, 0, -2.77555756156289e-17, 0, -1.66533453693773e-16, 0, 0, 0, 178, 0), .Dim = c(5L, 3L)), value = NULL)"));        
do.call(`dimnames<-`, argv);        
}, o=expected);        

