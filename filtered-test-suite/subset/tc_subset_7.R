expected <- eval(parse(text="structure(list(c = c(TRUE, NA, FALSE, TRUE)), .Names = \"c\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(a = c(1L, 2L, 3L, NA), b = c(NA, 3.14159265358979, 3.14159265358979, 3.14159265358979), c = c(TRUE, NA, FALSE, TRUE), d = c(\"aa\", \"bb\", NA, \"dd\"), e = structure(c(\"a1\", NA, NA, \"a4\"), class = \"AsIs\"), f = c(\"20010101\", NA, NA, \"20041026\")), .Names = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"), row.names = c(NA, -4L), class = \"data.frame\"), 3L)"));                
do.call(`.subset`, argv);                
}, o=expected);                

