expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE), .Dim = 3L, .Dimnames = structure(list(c(\"1\", \"2\", NA)), .Names = \"\")))"));      
do.call(`any`, argv);      
}, o=expected);      

