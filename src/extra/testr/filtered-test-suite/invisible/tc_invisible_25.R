expected <- eval(parse(text="structure(list(Df = c(1, 1, NA, 2), Deviance = c(12.2441566485997, 28.4640218366572, 32.825622681839, 32.4303239692005), AIC = c(73.9421143635373, 90.1619795515948, 92.5235803967766, 96.1282816841381)), .Names = c(\"Df\", \"Deviance\", \"AIC\"), row.names = c(\"+ M.user\", \"+ Temp\", \"<none>\", \"+ Soft\"), class = c(\"anova\", \"data.frame\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(Df = c(1, 1, NA, 2), Deviance = c(12.2441566485997, 28.4640218366572, 32.825622681839, 32.4303239692005), AIC = c(73.9421143635373, 90.1619795515948, 92.5235803967766, 96.1282816841381)), .Names = c(\"Df\", \"Deviance\", \"AIC\"), row.names = c(\"+ M.user\", \"+ Temp\", \"<none>\", \"+ Soft\"), class = c(\"anova\", \"data.frame\")))"));      
do.call(`invisible`, argv);      
}, o=expected);      

