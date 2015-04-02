expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 2, 3), .Dim = c(3L, 2L), .Dimnames = list(NULL, c(\"I\", \"a\")), foo = \"bar\", class = \"matrix\"), structure(c(1, 1, 1, 1, 2, 3), .Dim = c(3L, 2L), class = \"matrix\", foo = \"bar\", .Dimnames = list(NULL, c(\"I\", \"a\"))), TRUE, TRUE, FALSE, TRUE, FALSE)"));             
.Internal(identical(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

