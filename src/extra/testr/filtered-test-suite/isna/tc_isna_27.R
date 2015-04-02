expected <- eval(parse(text="structure(c(TRUE, FALSE), .Dim = c(2L, 1L), .Dimnames = list(c(\"419\", \"420\"), \"conc\"))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(conc = c(NA, 3.6)), .Names = \"conc\", row.names = 419:420, class = \"data.frame\"))"));                
do.call(`is.na`, argv);                
}, o=expected);                

