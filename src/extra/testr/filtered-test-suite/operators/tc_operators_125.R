expected <- eval(parse(text="structure(c(TRUE, TRUE), .Dim = 1:2, .Dimnames = list(\"ab\", c(\"cde\", \"cd\")))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(cde = 2L, cd = 4L), .Names = c(\"cde\", \"cd\"), row.names = \"ab\", class = \"data.frame\"), c(2, 4))"));            
do.call(`==`, argv);            
}, o=expected);            

