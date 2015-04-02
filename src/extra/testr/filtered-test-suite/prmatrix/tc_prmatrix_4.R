expected <- eval(parse(text="structure(c(952L, 3622L, 202L, 406L), .Dim = c(2L, 2L), .Dimnames = list(c(\"subcohort\", \"cohort\"), c(\"1\", \"2\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(952L, 3622L, 202L, 406L), .Dim = c(2L, 2L), .Dimnames = list(c(\"subcohort\", \"cohort\"), c(\"1\", \"2\"))), c(\"subcohort\", \"cohort\"), c(\"1\", \"2\"), FALSE, FALSE, NULL)"));     
.Internal(prmatrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));     
}, o=expected);     

