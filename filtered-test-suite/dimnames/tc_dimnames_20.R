expected <- eval(parse(text="list(c(\"<none>\", \"- M.user\", \"+ Temp\", \"+ Soft\"), c(\"Df\", \"Deviance\", \"AIC\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(Df = c(NA, 1, 1, 2), Deviance = c(12.2441566485997, 32.825622681839, 8.44399377410362, 11.9670615295804), AIC = c(73.9421143635373, 92.5235803967766, 72.1419514890412, 77.665019244518)), .Names = c(\"Df\", \"Deviance\", \"AIC\"), row.names = c(\"<none>\", \"- M.user\", \"+ Temp\", \"+ Soft\"), class = c(\"anova\", \"data.frame\")))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

