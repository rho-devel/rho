expected <- eval(parse(text="structure(c(\"-3.001e+155\", \"-1.067e+107\", \" -1.976e+62\", \"-9.961e+152\", \" -2.059e+23\", \"  1.000e+00\"), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"-3.001e+155\", \"-1.067e+107\", \" -1.976e+62\", \"-9.961e+152\", \" -2.059e+23\", \"  1.000e+00\"), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\")), NULL, FALSE, NULL, NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

