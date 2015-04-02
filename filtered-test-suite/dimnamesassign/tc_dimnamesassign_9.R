expected <- eval(parse(text="structure(c(1259, 1360, 845, 1053, 719, 774, 390, 413), .Dim = c(2L, 4L), .Dimnames = list(c(\"a\", \"b\"), NULL))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(1259, 1360, 845, 1053, 719, 774, 390, 413), .Dim = c(2L, 4L), .Dimnames = list(c(\"a\", \"b\"), NULL)), value = list(c(\"a\", \"b\")))"));  
do.call(`dimnames<-`, argv);  
}, o=expected);  

