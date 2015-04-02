expected <- eval(parse(text="c(NA, 2L, 4L, 7L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(V1 = structure(c(4L, 1L, 2L, 3L), .Label = c(\"1\", \"3\", \"6\", \"head\"), class = \"factor\"), V2 = c(NA, 2L, 4L, 7L), V3 = c(NA, NA, 5L, 8L), V4 = c(NA, NA, NA, 9L)), .Names = c(\"V1\", \"V2\", \"V3\", \"V4\"), class = \"data.frame\", row.names = c(NA, -4L)), 2L)"));       
do.call(`.subset2`, argv);       
}, o=expected);       

