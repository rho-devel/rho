expected <- eval(parse(text="structure(list(object = c(\"FALSE\", \"TRUE\", NA)), .Names = \"object\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(28L, 138L, 16L), .Dim = 3L, .Dimnames = structure(list(object = c(\"FALSE\", \"TRUE\", NA)), .Names = \"object\"), class = \"table\"))"));               
do.call(`dimnames`, argv);               
}, o=expected);               

