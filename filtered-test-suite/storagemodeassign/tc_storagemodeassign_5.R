expected <- eval(parse(text="structure(c(2.5, 0, 0.75, 0, 2.5, -2.5, 0.75, -2.5, 2.8), .Dim = c(3L, 3L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(2.5, 0, 0.75, 0, 2.5, -2.5, 0.75, -2.5, 2.8), .Dim = c(3L, 3L)), value = \"double\")"));  
do.call(`storage.mode<-`, argv);  
}, o=expected);  

