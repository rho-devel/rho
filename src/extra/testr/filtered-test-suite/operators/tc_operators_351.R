expected <- eval(parse(text="structure(list(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), fac = structure(c(1L, 2L, 2L, 3L, 1L, 3L, 3L, 2L, 2L, 1L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\"), char = structure(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\"), class = \"AsIs\")), .Names = c(\"x\", \"y\", \"fac\", \"char\"), row.names = c(NA, -10L), class = \"data.frame\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), fac = structure(c(1L, 2L, 2L, 3L, 1L, 3L, 3L, 2L, 2L, 1L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\"), char = structure(c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\"), class = \"AsIs\")), .Names = c(\"x\", \"y\", \"fac\", \"char\"), row.names = c(NA, -10L), class = \"data.frame\"))"));               
do.call(`(`, argv);               
}, o=expected);               

