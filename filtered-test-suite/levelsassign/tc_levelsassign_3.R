expected <- eval(parse(text="structure(c(1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L), .Label = c(\"A\", \"B\"), class = \"factor\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(1L, 1L, 2L, 2L, 3L, 3L, 1L, 1L, 2L, 2L, 3L, 3L), .Label = c(\"1\", \"2\", \"3\"), class = \"factor\"), value = structure(list(A = c(1, 3), B = 2), .Names = c(\"A\", \"B\")))"));   
do.call(`levels<-`, argv);   
}, o=expected);   

