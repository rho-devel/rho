expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE), .Dim = c(16L, 1L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\", \"11\", \"12\", \"13\", \"14\", \"15\", \"16\"), \"conc\"))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(conc = c(NA, NA, NA, NA, NA, NA, NA, 1.4, NA, NA, NA, NA, NA, NA, NA, 3)), .Names = \"conc\", row.names = c(NA, 16L), class = \"data.frame\"))"));                
do.call(`is.na`, argv);                
}, o=expected);                

