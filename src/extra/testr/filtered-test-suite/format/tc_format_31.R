expected <- eval(parse(text="structure(c(\"       -Inf\", \"       -Inf\", \"-2.248e+263\", \"       -Inf\", \"-3.777e+116\", \" -1.000e+00\"), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(-Inf, -Inf, -2.248e+263, -Inf, -3.777e+116, -1), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\")), FALSE, 7L, 0L, NULL, 3L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

