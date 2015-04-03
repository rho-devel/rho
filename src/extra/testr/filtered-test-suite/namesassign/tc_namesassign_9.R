expected <- eval(parse(text="structure(c(NA, FALSE, TRUE), .Names = c(NA, \"FALSE\", \"TRUE\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(NA, FALSE, TRUE), .Names = c(NA, \"FALSE\", \"TRUE\")), value = c(NA, \"FALSE\", \"TRUE\"))"));       
do.call(`names<-`, argv);       
}, o=expected);       

