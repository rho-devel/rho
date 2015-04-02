expected <- eval(parse(text="structure(c(3.04452243772342, 2.59026716544583, 2.75153531304195, 3.04452243772342, 2.59026716544583, 2.75153531304195, 3.04452243772342, 2.59026716544583, 2.75153531304195), .Dim = c(9L, 1L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\"), NULL))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), .Dim = c(9L, 5L), .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\"), c(\"(Intercept)\", \"outcome2\", \"outcome3\", \"treatment2\", \"treatment3\")), assign = c(0L, 1L, 1L, 2L, 2L), contrasts = structure(list(outcome = \"contr.treatment\", treatment = \"contr.treatment\"), .Names = c(\"outcome\", \"treatment\"))), c(3.04452243772342, -0.454255272277594, -0.292987124681473, 1.33790930192987e-15, 1.42108546079721e-15))"));              
do.call(`%*%`, argv);              
}, o=expected);              

