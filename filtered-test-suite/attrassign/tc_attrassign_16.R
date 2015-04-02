expected <- eval(parse(text="structure(c(0, -187, -34, 0, 165, 0, -95, 121, 107, 0, 41, 0, 0, 93, 0), .Dim = c(5L, 3L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(0, -187, -34, 0, 165, 0, -95, 121, 107, 0, 41, 0, 0, 93, 0), .Dim = c(5L, 3L)), \"dimnames\", value = NULL)"));           
do.call(`attr<-`, argv);           
}, o=expected);           

