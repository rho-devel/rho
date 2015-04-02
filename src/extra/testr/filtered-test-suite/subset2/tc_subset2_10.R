expected <- eval(parse(text="1L"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(V1 = 1L, V2 = 2L, V3 = 3L, V4 = structure(1L, .Label = \"\\abc\", class = \"factor\")), .Names = c(\"V1\", \"V2\", \"V3\", \"V4\"), class = \"data.frame\", row.names = c(NA, -1L)), 1L)"));       
do.call(`.subset2`, argv);       
}, o=expected);       

