expected <- eval(parse(text="list(c(\"<none>\", \"- M.user:Temp\", \"+ Soft\"), c(\"Df\", \"Deviance\", \"AIC\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(NA, 1, 2, 5.65604443125997, 8.44399377410362, 5.49523049516867, 71.3540021461976, 72.1419514890413, 75.1931882101063), .Dim = c(3L, 3L), .Dimnames = list(c(\"<none>\", \"- M.user:Temp\", \"+ Soft\"), c(\"Df\", \"Deviance\", \"AIC\"))))"));      
do.call(`dimnames`, argv);      
}, o=expected);      

