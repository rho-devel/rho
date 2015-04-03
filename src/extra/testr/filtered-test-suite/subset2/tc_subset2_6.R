expected <- eval(parse(text="c(10.5814458637509, 5.12914107700115, 5.12914107700115)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(Df = c(NA, 2L, 2L), Deviance = c(NA, 5.45230478674972, 2.66453525910038e-15), `Resid. Df` = c(8L, 6L, 4L), `Resid. Dev` = c(10.5814458637509, 5.12914107700115, 5.12914107700115)), .Names = c(\"Df\", \"Deviance\", \"Resid. Df\", \"Resid. Dev\"), row.names = c(\"NULL\", \"outcome\", \"treatment\"), class = c(\"anova\", \"data.frame\"), heading = \"Analysis of Deviance Table\\n\\nModel: poisson, link: log\\n\\nResponse: counts\\n\\nTerms added sequentially (first to last)\\n\\n\"), 4L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

