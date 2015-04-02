expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(4L, 3L, 2L, 1L, 2L), .Label = c(\"0.6\", \"0.8\", \"Area Examined\", \"C2\"), class = \"factor\"))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

