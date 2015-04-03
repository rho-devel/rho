expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = 8L, .Dimnames = structure(list(statef = c(\"act\", \"nsw\", \"nt\", \"qld\", \"sa\", \"tas\", \"vic\", \"wa\")), .Names = \"statef\"))"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(2L, 6L, 2L, 5L, 4L, 2L, 5L, 4L), .Dim = 8L, .Dimnames = structure(list(statef = c(\"act\", \"nsw\", \"nt\", \"qld\", \"sa\", \"tas\", \"vic\", \"wa\")), .Names = \"statef\"), class = \"table\"))"));                
do.call(`is.na`, argv);                
}, o=expected);                

