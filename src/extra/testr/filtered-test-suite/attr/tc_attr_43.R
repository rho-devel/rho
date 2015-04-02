expected <- eval(parse(text="c(\"NA\", \"a\", \"b\", \"c\", \"d\", NA)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(2L, NA, NA, 4L, 3L, 2L, 1L, 5L, 5L, 6L), .Label = c(\"NA\", \"a\", \"b\", \"c\", \"d\", NA), class = \"factor\"), \"levels\")"));        
do.call(`attr`, argv);        
}, o=expected);        

