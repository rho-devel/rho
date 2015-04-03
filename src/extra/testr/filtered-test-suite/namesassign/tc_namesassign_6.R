expected <- eval(parse(text="structure(1:3, .Names = c(NA, \"b\", NA))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:3, .Names = c(NA, \"b\", NA)), value = c(NA, \"b\"))"));       
do.call(`names<-`, argv);       
}, o=expected);       

