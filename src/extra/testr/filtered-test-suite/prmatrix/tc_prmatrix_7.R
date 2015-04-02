expected <- eval(parse(text="structure(c(\" 0.228763\", \"-0.000666\", \"\", \"0.08909\", \"0.00426\", \"\", \"0.08899\", \"0.00426\", \"\", \"6.59\", \"0.02\", \"6.02\", \"1.00\", \"1.00\", \"3.06\", \"0.01\", \"0.88\", \"0.12\"), .Dim = c(3L, 6L), .Dimnames = list(c(\"male\", \"tt(agechf), linear\", \"tt(agechf), nonlin\"), c(\"coef\", \"se(coef)\", \"se2\", \"Chisq\", \"DF\", \"p\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\" 0.228763\", \"-0.000666\", \"\", \"0.08909\", \"0.00426\", \"\", \"0.08899\", \"0.00426\", \"\", \"6.59\", \"0.02\", \"6.02\", \"1.00\", \"1.00\", \"3.06\", \"0.01\", \"0.88\", \"0.12\"), .Dim = c(3L, 6L), .Dimnames = list(c(\"male\", \"tt(agechf), linear\", \"tt(agechf), nonlin\"), c(\"coef\", \"se(coef)\", \"se2\", \"Chisq\", \"DF\", \"p\"))), c(\"male\", \"tt(agechf), linear\", \"tt(agechf), nonlin\"), c(\"coef\", \"se(coef)\", \"se2\", \"Chisq\", \"DF\", \"p\"), FALSE, FALSE, NULL)"));     
.Internal(prmatrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));     
}, o=expected);     

