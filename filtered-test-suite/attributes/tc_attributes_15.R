expected <- eval(parse(text="structure(list(levels = c(\"1\", \"2\", NA)), .Names = \"levels\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1L, NA, 3L), .Label = c(\"1\", \"2\", NA)))"));               
do.call(`attributes`, argv);               
}, o=expected);               

