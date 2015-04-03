expected <- eval(parse(text="c(1.1+0i, NA, 3+0i)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(A = c(1, NA, 1), B = c(1.1, NA, 2), C = c(1.1+0i, NA, 3+0i), D = c(NA_integer_, NA_integer_, NA_integer_), E = c(FALSE, NA, TRUE), F = c(\"abc\", NA, \"def\")), .Names = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\"), class = \"data.frame\", row.names = c(\"1\", \"2\", \"3\")), 3L)"));       
do.call(`.subset2`, argv);       
}, o=expected);       

