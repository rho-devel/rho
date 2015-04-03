expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(Df = c(1L, 7L), `Sum Sq` = c(158.407612694902, 204.202165082876), `Mean Sq` = c(158.407612694902, 29.1717378689823), `F value` = c(5.43017400630538, NA), `Pr(>F)` = c(0.052592726218915, NA)), .Names = c(\"Df\", \"Sum Sq\", \"Mean Sq\", \"F value\", \"Pr(>F)\"), row.names = c(\"depression\", \"Residuals\"), class = c(\"anova\", \"data.frame\"), heading = c(\"Analysis of Variance Table\\n\", \"Response: weight\")))"));            
do.call(`is.recursive`, argv);            
}, o=expected);            

