expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(Df = c(NA, 1), Deviance = c(12.2441566485997, 32.825622681839), AIC = c(73.9421143635373, 92.5235803967766)), .Names = c(\"Df\", \"Deviance\", \"AIC\"), row.names = c(\"<none>\", \"- M.user\"), class = c(\"anova\", \"data.frame\"), heading = c(\"Single term deletions\", \"\\nModel:\", \"cbind(X, M) ~ M.user\")))"));      
do.call(`is.matrix`, argv);      
}, o=expected);      

