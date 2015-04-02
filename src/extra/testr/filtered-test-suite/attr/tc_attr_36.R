expected <- eval(parse(text="NULL"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0, 0, 0, 1.90299264808673e-318, 0, 0, 0, 0, 0), .Dim = c(1L, 12L), .Dimnames = list(NULL, c(\"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\"))), \"class\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

