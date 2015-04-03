expected <- eval(parse(text="c(FALSE, TRUE, TRUE, TRUE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(0, 1, 2, 2), .Dim = c(4L, 1L), .Dimnames = list(c(\"Y\", \"B\", \"V\", \"N\"), NULL)))"));  
do.call(`as.logical`, argv);  
}, o=expected);  

