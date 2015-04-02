expected <- eval(parse(text="structure(c(1, 1, 1, 1, 2, 3), .Dim = c(3L, 2L), class = \"matrix\", foo = \"bar\", .Dimnames = list(NULL, c(\"I\", \"a\")))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 2, 3), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"I\", \"a\")), foo = \"bar\", class = \"matrix\"), value = structure(list(class = \"matrix\", foo = \"bar\", dimnames = list(NULL, c(\"I\", \"a\")), dim = c(3L, 2L)), .Names = c(\"class\", \"foo\", \"dimnames\", \"dim\")))"));   
do.call(`attributes<-`, argv);   
}, o=expected);   

