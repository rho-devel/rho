expected <- eval(parse(text="c(TRUE, TRUE, NA)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(1L, 2L, NA), .Label = c(\"1\", \"2\"), class = \"factor\"), structure(c(1L, 2L, NA), .Label = c(\"1\", \"2\"), class = \"factor\"))"));   
do.call(`==`, argv);   
}, o=expected);   

