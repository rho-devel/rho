expected <- eval(parse(text="structure(list(X1.10 = 1:10, z = structure(list(x = 1:10, yyy = 11:20), .Names = c(\"x\", \"yyy\"), row.names = c(NA, -10L), class = \"data.frame\")), .Names = c(\"X1.10\", \"z\"), row.names = c(NA, -10L))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(X1.10 = 1:10, z = structure(list(x = 1:10, yyy = 11:20), .Names = c(\"x\", \"yyy\"), row.names = c(NA, -10L), class = \"data.frame\")), .Names = c(\"X1.10\", \"z\"), row.names = c(NA, -10L)))"));     
do.call(`unclass`, argv);     
}, o=expected);     

