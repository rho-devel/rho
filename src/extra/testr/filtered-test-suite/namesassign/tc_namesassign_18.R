expected <- eval(parse(text="structure(c(67L, 34L), .Dim = 2L, .Dimnames = list(c(\"\\\"actual\\\"\", \"virtual\")), class = \"table\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(67L, 34L), .Dim = 2L, .Dimnames = list(c(\"\\\"actual\\\"\", \"virtual\")), class = \"table\"), value = c(\"\\\"actual\\\"\", \"virtual\"))"));       
do.call(`names<-`, argv);       
}, o=expected);       

