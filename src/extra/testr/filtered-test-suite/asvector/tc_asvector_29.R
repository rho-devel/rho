expected <- eval(parse(text="1:12"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:12, .Dim = 3:4, .Dimnames = list(c(\"A\", \"B\", \"C\"), c(\"D\", \"E\", \"F\", \"G\"))), \"any\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

