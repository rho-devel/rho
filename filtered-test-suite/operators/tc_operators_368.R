expected <- eval(parse(text="structure(c(12L, 46L), class = \"table\", .Dim = 2L, .Dimnames = structure(list(c(\"1\", \"2\")), .Names = \"\"))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(12L, 23L), .Dim = 2L, .Dimnames = structure(list(c(\"1\", \"2\")), .Names = \"\"), class = \"table\"), 1:2)"));                
do.call(`*`, argv);                
}, o=expected);                

