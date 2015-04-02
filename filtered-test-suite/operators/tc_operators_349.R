expected <- eval(parse(text="structure(list(x = numeric(0), y = numeric(0), fac = structure(integer(0), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = c(\"x\", \"y\", \"fac\"), row.names = integer(0), class = \"data.frame\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(x = numeric(0), y = numeric(0), fac = structure(integer(0), .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = c(\"x\", \"y\", \"fac\"), row.names = integer(0), class = \"data.frame\"))"));               
do.call(`(`, argv);               
}, o=expected);               

