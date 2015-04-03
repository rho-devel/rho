expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 2L, 1L, NA), .Label = c(\"no\", \"yes\"), class = \"factor\"))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

