expected <- eval(parse(text="structure(c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_), .Dim = c(1L, 5L), .Dimnames = list(\"Residuals\", c(\"Df\", \"Sum Sq\", \"Mean Sq\", \"F value\", \"Pr(>F)\")))"));    
test(id=0, code={    
argv <- eval(parse(text="list(NA_real_, 1L, 5L, FALSE, list(\"Residuals\", c(\"Df\", \"Sum Sq\", \"Mean Sq\", \"F value\", \"Pr(>F)\")), FALSE, FALSE)"));    
.Internal(`matrix`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));    
}, o=expected);    

