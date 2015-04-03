expected <- eval(parse(text="structure(list(NULL, NULL, NULL, NULL, NULL, NULL), .Dim = 2:3)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(NULL, NULL, NULL, NULL, NULL, NULL), .Dim = 2:3), value = c(2, 3))"));  
do.call(`dim<-`, argv);  
}, o=expected);  

