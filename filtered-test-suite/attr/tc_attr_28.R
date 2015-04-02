expected <- eval(parse(text="structure(c(\"variable1\", \"variable2\"), .Names = c(\"variable1\", \"variable2\"))"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(variable1 = c(1, 2, 2), variable2 = c(1, 1, 3)), .Names = c(\"variable1\", \"variable2\"), row.names = c(NA, -3L), class = \"data.frame\", variable.labels = structure(c(\"variable1\", \"variable2\"), .Names = c(\"variable1\", \"variable2\")), codepage = 20127L), \"variable.labels\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

