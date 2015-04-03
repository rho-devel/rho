expected <- eval(parse(text="structure(NA, .Dim = c(1L, 1L))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(NA, .Dim = c(1L, 1L)), value = c(1L, 1L))"));   
do.call(`dim<-`, argv);   
}, o=expected);   

