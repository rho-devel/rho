expected <- eval(parse(text="structure(list(AIC = c(92.5235803967766, 73.9421143635373, 90.1619795515948, 96.1282816841381)), .Names = \"AIC\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(Df = c(NA, 1, 1, 2), Deviance = c(32.825622681839, 12.2441566485997, 28.4640218366572, 32.4303239692005), AIC = c(92.5235803967766, 73.9421143635373, 90.1619795515948, 96.1282816841381)), .Names = c(\"Df\", \"Deviance\", \"AIC\"), row.names = c(\"<none>\", \"+ M.user\", \"+ Temp\", \"+ Soft\"), class = c(\"anova\", \"data.frame\")), 3L)"));   
do.call(`.subset`, argv);   
}, o=expected);   

