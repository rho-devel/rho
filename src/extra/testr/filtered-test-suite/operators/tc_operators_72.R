expected <- eval(parse(text="structure(c(1L, 1L, 2L, 5L, 6L, 6L), .Label = c(\"1:1\", \"1:2\", \"1:3\", \"2:1\", \"2:2\", \"2:3\"), class = \"factor\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 2L, 2L, 2L), .Label = c(\"1\", \"2\"), class = \"factor\"), structure(c(1L, 1L, 2L, 2L, 3L, 3L), .Label = c(\"1\", \"2\", \"3\"), class = \"factor\"))"));             
do.call(`:`, argv);             
}, o=expected);             

