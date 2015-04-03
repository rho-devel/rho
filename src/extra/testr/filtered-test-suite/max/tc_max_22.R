expected <- eval(parse(text="7L"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(7L, 4L, 3L), .Dim = 3L, .Dimnames = structure(list(c(\"0\", \"1\", \"5\")), .Names = \"\"), class = \"table\"))"));      
do.call(`max`, argv);      
}, o=expected);      

