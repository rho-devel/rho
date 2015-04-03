expected <- eval(parse(text="structure(c(-99, 123, 0, -27, 0, 136, 3.5527136788005e-14, 0, -89, -59, 54.9999999999999, -260, 30, 47, 0), .Dim = c(5L, 3L))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(-99, 123, 0, -27, 0, 136, 3.5527136788005e-14, 0, -89, -59, 54.9999999999999, -260, 30, 47, 0), .Dim = c(5L, 3L)), \"dimnames\", value = NULL)"));           
do.call(`attr<-`, argv);           
}, o=expected);           

