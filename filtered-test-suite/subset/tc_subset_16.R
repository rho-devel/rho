expected <- eval(parse(text="structure(list(VAR1 = c(1, 2, 3, 4, 5), VAR3 = c(1, 1, 1, 1, NA)), .Names = c(\"VAR1\", \"VAR3\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(VAR1 = c(1, 2, 3, 4, 5), VAR2 = c(5, 4, 3, 2, 1), VAR3 = c(1, 1, 1, 1, NA)), .Names = c(\"VAR1\", \"VAR2\", \"VAR3\"), row.names = c(NA, -5L), class = \"data.frame\"), c(1, 3))"));   
do.call(`.subset`, argv);   
}, o=expected);   

