expected <- eval(parse(text="structure(list(names = c(\"x\", \"y\", \"fac\"), row.names = 1:10, class = \"data.frame\"), .Names = c(\"names\", \"row.names\", \"class\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), fac = structure(c(1L, 3L, 2L, 3L, 3L, 1L, 2L, 3L, 2L, 2L), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = c(\"x\", \"y\", \"fac\"), row.names = c(NA, -10L), class = \"data.frame\"))"));       
do.call(`attributes`, argv);       
}, o=expected);       

