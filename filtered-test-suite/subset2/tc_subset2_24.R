expected <- eval(parse(text="c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4), x1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10), x2 = c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68), x4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12)), .Names = c(\"y\", \"x1\", \"x2\", \"x4\"), class = \"data.frame\", row.names = c(NA, 13L), terms = quote(y ~ x1 + x2 + x4)), 3L)"));       
do.call(`.subset2`, argv);       
}, o=expected);       

