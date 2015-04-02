expected <- eval(parse(text="structure(c(31.9166666666667, -5.77777777777778, -10.4101831674686, -2.63888888888889, NA), .Dim = c(5L, 1L), .Dimnames = list(c(\"(Intercept)\", \"woolB\", \"tens.L\", \"tensionM\", \"tensionH\"), NULL))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(31.9166666666667, -5.77777777777778, -10.4101831674686, -2.63888888888889, NA), .Names = c(\"(Intercept)\", \"woolB\", \"tens.L\", \"tensionM\", \"tensionH\")), c(5L, 1L), list(c(\"(Intercept)\", \"woolB\", \"tens.L\", \"tensionM\", \"tensionH\"), NULL))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

