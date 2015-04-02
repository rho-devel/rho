expected <- eval(parse(text="structure(c(-5.3088868291531, 5.2393213877113, -5.301817110509, 5.29234872074472), .Dim = c(2L, 2L), .Dimnames = list(c(\"5%\", \"95%\"), NULL))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(-5.3088868291531, 5.2393213877113, -5.301817110509, 5.29234872074472), .Names = c(\"5%\", \"95%\", \"5%\", \"95%\")), c(2, 2), list(c(\"5%\", \"95%\"), NULL))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

