expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = 5:6, .Dimnames = structure(list(blocks = c(\"1\", \"2\", \"3\", \"4\", \"5\"), varieties = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\")), .Names = c(\"blocks\", \"varieties\")))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), .Dim = 5:6), structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Dim = 5:6, .Dimnames = structure(list(blocks = c(\"1\", \"2\", \"3\", \"4\", \"5\"), varieties = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\")), .Names = c(\"blocks\", \"varieties\")), class = \"table\"))"));            
do.call(`==`, argv);            
}, o=expected);            

