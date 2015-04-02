expected <- eval(parse(text="3L"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(Df = c(NA, 1, 2), Deviance = c(12.2441566485997, 8.44399377410362, 11.9670615295804), AIC = c(73.9421143635373, 72.1419514890412, 77.665019244518)), .Names = c(\"Df\", \"Deviance\", \"AIC\"), row.names = c(\"<none>\", \"Temp\", \"Soft\"), class = c(\"anova\", \"data.frame\"), heading = c(\"Single term additions\", \"\\nModel:\", \"cbind(X, M) ~ M.user\")), 2L)"));       
.Internal(`shortRowNames`(argv[[1]], argv[[2]]));       
}, o=expected);       

