expected <- eval(parse(text="structure(list(`1` = 0:10, `3` = 20:30), .Names = c(\"1\", \"3\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(`1` = 0:10, `2` = 10:20, `3` = 20:30), .Names = c(\"1\", \"2\", \"3\"), row.names = c(NA, -11L), class = \"data.frame\"), -2)"));   
do.call(`.subset`, argv);   
}, o=expected);   

