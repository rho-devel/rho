expected <- eval(parse(text="c(TRUE, NA, NA, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(2L, NA, NA, 4L, 3L, 2L, 1L, 5L, 5L, 6L), .Label = c(\"NA\", \"a\", \"b\", \"c\", \"d\", NA), class = \"factor\"), structure(c(2L, NA, NA, 4L, 3L, 2L, 1L, 5L, 5L, 6L), .Label = c(\"NA\", \"a\", \"b\", \"c\", \"d\", NA), class = \"factor\"))"));   
do.call(`==`, argv);   
}, o=expected);   

