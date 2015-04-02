expected <- eval(parse(text="structure(list(1L, 3.14159265358979, 3+5i, \"testit\", TRUE, structure(1L, .Label = \"foo\", class = \"factor\")), .Dim = c(1L, 6L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(list(1L, 3.14159265358979, 3+5i, \"testit\", TRUE, structure(1L, .Label = \"foo\", class = \"factor\")), .Dim = c(1L, 6L)), value = c(1, 6))"));  
do.call(`dim<-`, argv);  
}, o=expected);  

