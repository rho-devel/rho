expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = 9L, .Dimnames = structure(list(x = c(\"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\")), .Names = \"x\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(9L, 4L, 6L, 5L, 3L, 10L, 5L, 3L, 5L), .Dim = 9L, .Dimnames = structure(list(x = c(\"0\", \"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\")), .Names = \"x\"), class = \"table\"), c(9, 4, 6, 5, 3, 10, 5, 3, 5))"));            
do.call(`==`, argv);            
}, o=expected);            

