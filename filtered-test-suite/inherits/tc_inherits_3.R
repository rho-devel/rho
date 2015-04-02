expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11), .Tsp = c(2, 11, 1)), \"data.frame\", FALSE)"));         
.Internal(`inherits`(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

