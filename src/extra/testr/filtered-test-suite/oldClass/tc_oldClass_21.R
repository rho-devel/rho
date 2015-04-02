expected <- eval(parse(text="\"data.frame\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(Model = 1:2, df = c(5, 6), AIC = c(\"1571.455\", \"1570.925\"), BIC = c(\"1590.056\", \"1593.247\"), logLik = c(-780.727255295109, -779.462624623506), Test = structure(1:2, .Label = c(\"\", \"1 vs 2\"), class = \"factor\"), L.Ratio = c(NA, 2.52926134320705), `p-value` = c(NA, 0.111752518719366)), .Names = c(\"Model\", \"df\", \"AIC\", \"BIC\", \"logLik\", \"Test\", \"L.Ratio\", \"p-value\"), row.names = c(\"fm1\", \"fm2\"), class = \"data.frame\"))"));                
do.call(`oldClass`, argv);                
}, o=expected);                

