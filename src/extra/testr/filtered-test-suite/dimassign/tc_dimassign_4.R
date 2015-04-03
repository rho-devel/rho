expected <- eval(parse(text="structure(list(1:3, 4:6, 3.14159265358979, c(\"a\", \"b\", \"c\")), .Dim = c(2L, 2L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(1:3, 4:6, 3.14159265358979, c(\"a\", \"b\", \"c\")), .Dim = c(2L, 2L)), value = c(2, 2))"));  
do.call(`dim<-`, argv);  
}, o=expected);  

