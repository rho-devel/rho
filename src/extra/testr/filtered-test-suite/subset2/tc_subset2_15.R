expected <- eval(parse(text="20:30"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(A = 0:10, `NA` = 20:30), .Names = c(\"A\", NA), class = \"data.frame\", row.names = c(NA, -11L)), 2L)"));       
do.call(`.subset2`, argv);       
}, o=expected);       

