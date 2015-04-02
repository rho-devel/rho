expected <- eval(parse(text="structure(list(k = 0.005, g1 = 50, g2 = 550), .Names = c(\"k\", \"g1\", \"g2\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(0.005, 50, 550), .Names = c(\"k\", \"g1\", \"g2\")), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

