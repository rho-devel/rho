expected <- eval(parse(text="c(NA, 0.663893424608742)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(Res.Df = c(20, 21), RSS = c(652424.52183908, 658770.746755654), Df = c(NA, -1), `Sum of Sq` = c(NA, -6346.22491657443), F = c(NA, 0.194542807762205), `Pr(>F)` = c(NA, 0.663893424608742)), .Names = c(\"Res.Df\", \"RSS\", \"Df\", \"Sum of Sq\", \"F\", \"Pr(>F)\"), row.names = c(\"1\", \"2\"), class = c(\"anova\", \"data.frame\"), heading = c(\"Analysis of Variance Table\\n\", \"Model 1: birthw ~ sex + sex:age - 1\\nModel 2: birthw ~ sex + age - 1\")), 6L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

