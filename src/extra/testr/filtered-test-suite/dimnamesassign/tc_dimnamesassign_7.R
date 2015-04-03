expected <- eval(parse(text="structure(c(-0.0124410638457178, NA, 0.00669768951701377, NA, 0.00669754897238661, NA, 3.45036480545864, 2.52673085623929, 1, 2.64771226663238, 0.0632378108418818, 0.404928794321981), .Dim = c(2L, 6L), .Dimnames = list(c(\"linear\", \"nonlin\"), NULL))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(-0.0124410638457178, NA, 0.00669768951701377, NA, 0.00669754897238661, NA, 3.45036480545864, 2.52673085623929, 1, 2.64771226663238, 0.0632378108418818, 0.404928794321981), .Dim = c(2L, 6L), .Dimnames = list(c(\"linear\", \"nonlin\"), NULL)), value = list(c(\"linear\", \"nonlin\"), NULL))"));        
do.call(`dimnames<-`, argv);        
}, o=expected);        

