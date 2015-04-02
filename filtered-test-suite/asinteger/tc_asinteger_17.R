expected <- eval(parse(text="c(1L, 2L, 3L, 2L)"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(1L, 2L, 3L, 2L), .Label = c(\"1\", \"2\", NA), class = \"factor\"))"));    
do.call(`as.integer`, argv);    
}, o=expected);    

