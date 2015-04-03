expected <- eval(parse(text="structure(c(-0.223606797749979, 0, -0.670820393249937, 0.447213595499958), .Dim = c(2L, 2L), .Dimnames = list(c(\"Intercept\", \"X\"), NULL))"));                                 
test(id=0, code={                                 
argv <- eval(parse(text="list(structure(c(-4.47213595499958, 0, -6.70820393249937, 2.23606797749979), .Dim = c(2L, 2L), .Dimnames = list(NULL, c(\"Intercept\", \"X\"))), structure(c(1, 0, 0, 1), .Dim = c(2L, 2L)), 2.22044604925031e-16)"));                                 
.Internal(La_solve(argv[[1]], argv[[2]], argv[[3]]));                                 
}, o=expected);                                 

