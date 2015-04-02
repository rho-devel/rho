expected <- eval(parse(text="structure(c(0.000231493362923329, -0.000450438233072705), .Dim = c(2L, 1L))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(-0.91092349872819, -1.26769315823132, 0, -1.11965595698793), .Dim = c(2L, 2L)), structure(c(-0.000210872744086474, 0.000210873298561107), .Dim = c(2L, 1L)), 2L, FALSE, FALSE)"));       
.Internal(backsolve(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));       
}, o=expected);       

