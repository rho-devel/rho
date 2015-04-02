expected <- eval(parse(text="c(\"1\", \"2\", NA)"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(c(2L, 1L, 3L), .Label = c(\"1\", \"2\", NA), class = c(\"ordered\", \"factor\")), \"levels\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

