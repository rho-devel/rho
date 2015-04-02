expected <- eval(parse(text="c(\"A\", \"B\", NA)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(A = NULL, B = NULL, `NA` = NULL), .Names = c(\"A\", \"B\", NA)))"));         
do.call(`names`, argv);         
}, o=expected);         

