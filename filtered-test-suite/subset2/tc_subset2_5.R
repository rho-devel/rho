expected <- eval(parse(text="c(NA, NA)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(V1 = c(1L, 9L), V2 = c(NA, NA), V3 = c(23L, 87L), V4 = c(NA, 654L)), .Names = c(\"V1\", \"V2\", \"V3\", \"V4\"), class = \"data.frame\", row.names = c(NA, -2L)), 2L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

