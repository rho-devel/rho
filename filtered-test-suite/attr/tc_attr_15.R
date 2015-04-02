expected <- eval(parse(text="c(\"<none>\", \"- M.user:Temp\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(Df = c(NA, 1), Deviance = c(5.65604443125997, 8.44399377410362), AIC = c(71.3540021461976, 72.1419514890413)), .Names = c(\"Df\", \"Deviance\", \"AIC\"), row.names = c(\"<none>\", \"- M.user:Temp\"), class = c(\"anova\", \"data.frame\"), heading = c(\"Single term deletions\", \"\\nModel:\", \"cbind(X, M) ~ M.user + Temp + M.user:Temp\")), \"row.names\")"));        
do.call(`attr`, argv);        
}, o=expected);        

