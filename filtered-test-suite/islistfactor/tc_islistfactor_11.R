expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(structure(0, .Dim = c(1L, 1L)), structure(-4.9497224423095e-07, .Dim = c(1L, 1L)), structure(0, .Dim = c(1L, 1L)), structure(-7.44931694456399e-07, .Dim = c(1L, 1L))), FALSE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

