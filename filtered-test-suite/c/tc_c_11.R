expected <- eval(parse(text="structure(list(`(Intercept)` = \"(Intercept)\", B = \"B\", V = \"V\", N = \"N\", `V:N` = c(\"V\", \"N\"), Residuals = c(\"B\", \"V\", \"N\", \"Within\")), .Names = c(\"(Intercept)\", \"B\", \"V\", \"N\", \"V:N\", \"Residuals\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(`(Intercept)` = \"(Intercept)\", structure(list(B = \"B\", V = \"V\", N = \"N\", `V:N` = c(\"V\", \"N\"), Residuals = c(\"B\", \"V\", \"N\", \"Within\")), .Names = c(\"B\", \"V\", \"N\", \"V:N\", \"Residuals\")))"));        
do.call(`c`, argv);        
}, o=expected);        

