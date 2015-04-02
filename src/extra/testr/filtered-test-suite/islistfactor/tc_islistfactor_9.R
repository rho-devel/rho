expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(`1` = c(2, 1, 4, 3), `2` = c(3, 1.5, 5, 4, 1.5), `3` = c(6.5, 1.5, 9, 8, 1.5, 6.5, 4, 4, 4), `4` = c(7, 1.5, 10, 9, 1.5, 7, 4, 4, 4, 7)), .Dim = 4L, .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\"))), TRUE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

