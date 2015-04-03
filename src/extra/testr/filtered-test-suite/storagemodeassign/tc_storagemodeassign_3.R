expected <- eval(parse(text="structure(c(0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L), .Dim = c(4L, 4L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), .Dim = c(4L, 4L)), value = \"integer\")"));  
do.call(`storage.mode<-`, argv);  
}, o=expected);  

