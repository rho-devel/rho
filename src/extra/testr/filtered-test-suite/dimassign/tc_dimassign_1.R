expected <- eval(parse(text="structure(c(300, 3000, 400, 4000), .Dim = c(2L, 2L, 1L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(300, 3000, 400, 4000), .Dim = c(2L, 2L, 1L)), value = c(2, 2, 1))"));  
do.call(`dim<-`, argv);  
}, o=expected);  

