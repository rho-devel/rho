expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(V1 = c(NA, 2, NA, 4, 5), V2 = c(NA, NA, 3, 4, 5)), .Names = c(\"V1\", \"V2\"), class = \"data.frame\", row.names = c(NA, -5L)))"));                 
do.call(`is.matrix`, argv);                 
}, o=expected);                 

