expected <- eval(parse(text="c(\"     1.5\", \"13.34143\", \"   1e-15\", \"       8\", \"       1\", \"     500\", \"      28\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1.5, 13.3414265412268, 1e-15, 8, 1, 500, 28), .Dim = c(7L, 1L), .Dimnames = list(c(\"m.ship.expon.\", \"objective\", \"tolerance\", \"iterations\", \"converged\", \"maxit\", \"n\"), \" \")), \"double\", 8L, 7L, \"g\", \"\", c(15L, 15L, 15L, 15L, 15L, 15L, 15L))"));        
.Internal(formatC(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));        
}, o=expected);        

