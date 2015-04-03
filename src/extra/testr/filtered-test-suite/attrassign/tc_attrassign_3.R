expected <- eval(parse(text="structure(c(1, 0, 0, 0, NA, 1, 0, 0, 0, 7, 1, 0, 3, 0, 0, 1), .Dim = c(4L, 4L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(1, 0, 0, 0, NA, 1, 0, 0, 0, 7, 1, 0, 3, 0, 0, 1), .Dim = c(4L, 4L)), \"dimnames\", value = NULL)"));           
do.call(`attr<-`, argv);           
}, o=expected);           

