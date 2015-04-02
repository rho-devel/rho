expected <- eval(parse(text="structure(list(Df = structure(c(NA, 2, 1), .Names = c(\"<none>\", \"Soft\", \"M.user:Temp\")), Deviance = structure(c(8.44399377410362, 8.2279889309135, 5.65604443125997), .Names = c(\"<none>\", \"Soft\", \"M.user:Temp\")), AIC = structure(c(72.1419514890413, 75.9259466458512, 71.3540021461976), .Names = c(\"<none>\", \"Soft\", \"M.user:Temp\"))), .Names = c(\"Df\", \"Deviance\", \"AIC\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(Df = structure(c(NA, 2, 1), .Names = c(\"<none>\", \"Soft\", \"M.user:Temp\")), Deviance = structure(c(8.44399377410362, 8.2279889309135, 5.65604443125997), .Names = c(\"<none>\", \"Soft\", \"M.user:Temp\")), AIC = structure(c(72.1419514890413, 75.9259466458512, 71.3540021461976), .Names = c(\"<none>\", \"Soft\", \"M.user:Temp\")))"));         
do.call(`list`, argv);         
}, o=expected);         

