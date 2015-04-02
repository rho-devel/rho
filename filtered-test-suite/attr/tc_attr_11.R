expected <- eval(parse(text="structure(list(M.user = \"contr.treatment\", Temp = \"contr.treatment\"), .Names = c(\"M.user\", \"Temp\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0), .Dim = c(12L, 3L), .Dimnames = list(c(\"1\", \"3\", \"5\", \"7\", \"9\", \"11\", \"13\", \"15\", \"17\", \"19\", \"21\", \"23\"), c(\"(Intercept)\", \"M.userY\", \"TempLow\")), assign = 0:2, contrasts = structure(list(M.user = \"contr.treatment\", Temp = \"contr.treatment\"), .Names = c(\"M.user\", \"Temp\"))), \"contrasts\")"));        
do.call(`attr`, argv);        
}, o=expected);        

