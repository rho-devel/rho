expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(a = c(1L, 2L, 3L, NA), b = c(NA, 3.14159265358979, 3.14159265358979, 3.14159265358979), c = c(TRUE, NA, FALSE, TRUE), d = structure(c(1L, 2L, NA, 3L), .Label = c(\"aa\", \"bb\", \"dd\"), class = \"factor\"), e = structure(c(\"a1\", NA, NA, \"a4\"), class = \"AsIs\"), f = structure(c(11323, NA, NA, 12717), class = \"Date\")), .Names = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\"), row.names = c(NA, -4L), class = \"data.frame\"))"));       
do.call(`is.pairlist`, argv);       
}, o=expected);       

