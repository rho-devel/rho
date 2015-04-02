expected <- eval(parse(text="structure(c(15L, 29L), .Names = c(\"bibentry\", NA))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(15L, 14L), .Names = c(\"bibentry\", NA)))"));             
do.call(`cumsum`, argv);             
}, o=expected);             

