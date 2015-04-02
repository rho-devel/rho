expected <- eval(parse(text="structure(c(\" 0.00561\", \"-1.65487\", \"\", \"0.012\", \"0.483\", \"\", \"0.00872\", \"0.38527\", \"\", \" 0.22\", \"11.74\", \"20.33\", \" 1.0\", \" 1.0\", \"13.9\", \"0.64000\", \"0.00061\", \"0.12000\"), .Dim = c(3L, 6L), .Dimnames = list(c(\"age\", \"sex\", \"frailty(id, dist = \\\"t\\\", c\"), c(\"coef\", \"se(coef)\", \"se2\", \"Chisq\", \"DF\", \"p\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\" 0.00561\", \"-1.65487\", \"\", \"0.012\", \"0.483\", \"\", \"0.00872\", \"0.38527\", \"\", \" 0.22\", \"11.74\", \"20.33\", \" 1.0\", \" 1.0\", \"13.9\", \"0.64000\", \"0.00061\", \"0.12000\"), .Dim = c(3L, 6L), .Dimnames = list(c(\"age\", \"sex\", \"frailty(id, dist = \\\"t\\\", c\"), c(\"coef\", \"se(coef)\", \"se2\", \"Chisq\", \"DF\", \"p\"))), c(\"age\", \"sex\", \"frailty(id, dist = \\\"t\\\", c\"), c(\"coef\", \"se(coef)\", \"se2\", \"Chisq\", \"DF\", \"p\"), FALSE, FALSE, NULL)"));     
.Internal(prmatrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));     
}, o=expected);     

