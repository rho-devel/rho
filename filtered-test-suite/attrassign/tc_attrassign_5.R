expected <- eval(parse(text="structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = 3:4)"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = 3:4), \"dimnames\", value = NULL)"));           
do.call(`attr<-`, argv);           
}, o=expected);           

