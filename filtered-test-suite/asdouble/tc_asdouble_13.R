expected <- eval(parse(text="c(21, 16.4, 18.7, 16.8, 17.8, 10.9, 14, 3.5, 4.3, 3.5, 2.7, 6, 14, 2.3)"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(21, 16.4, 18.7, 16.8, 17.8, 10.9, 14, 3.5, 4.3, 3.5, 2.7, 6, 14, 2.3), .Dim = c(7L, 2L), .Dimnames = list(c(\"L\", \"NL\", \"D\", \"B\", \"F\", \"IRL\", \"UK\"), c(\"x\", \"y\"))))"));               
do.call(`as.double`, argv);               
}, o=expected);               

