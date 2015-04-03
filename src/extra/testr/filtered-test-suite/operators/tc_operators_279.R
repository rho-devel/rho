expected <- eval(parse(text="structure(c(4, 4, 4, 4), .Dim = c(2L, 2L), .Dimnames = list(c(\"Milk\", \"Tea\"), c(\"Milk\", \"Tea\")))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(2, -2, -2, 2), .Dim = c(2L, 2L), .Dimnames = list(c(\"Milk\", \"Tea\"), c(\"Milk\", \"Tea\"))), 2)"));              
do.call(`^`, argv);              
}, o=expected);              

