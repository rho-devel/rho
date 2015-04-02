expected <- eval(parse(text="structure(c(2L, 1L, 3L), .Label = c(\"NA\", \"a\", \"b\"), class = \"factor\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(2L, 1L, 3L), .Label = c(\"NA\", \"a\", \"b\"), class = \"factor\"))"));     
do.call(`(`, argv);     
}, o=expected);     

