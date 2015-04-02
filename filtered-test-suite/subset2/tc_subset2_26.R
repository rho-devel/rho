expected <- eval(parse(text="NA_real_"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(Df = 10L, `Sum Sq` = 2.74035772634541, `Mean Sq` = 0.274035772634541, `F value` = NA_real_, `Pr(>F)` = NA_real_), .Names = c(\"Df\", \"Sum Sq\", \"Mean Sq\", \"F value\", \"Pr(>F)\"), row.names = \"Residuals\", class = c(\"anova\", \"data.frame\"), heading = c(\"Analysis of Variance Table\\n\", \"Response: y\")), 5L)"));       
do.call(`.subset2`, argv);       
}, o=expected);       

