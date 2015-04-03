expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(V1 = 1L, V2 = structure(1L, .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), V3 = 6), .Names = c(\"V1\", \"V2\", \"V3\"), class = \"data.frame\", row.names = c(NA, -1L)))"));      
do.call(`is.matrix`, argv);      
}, o=expected);      

