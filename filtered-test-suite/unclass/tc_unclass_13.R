expected <- eval(parse(text="structure(list(Df = c(NA, 0L), Deviance = c(NA, 0), `Resid. Df` = c(10L, 10L), `Resid. Dev` = c(2.74035772634541, 2.74035772634541)), .Names = c(\"Df\", \"Deviance\", \"Resid. Df\", \"Resid. Dev\"), row.names = c(\"NULL\", \"x\"), heading = \"Analysis of Deviance Table\\n\\nModel: gaussian, link: identity\\n\\nResponse: y\\n\\nTerms added sequentially (first to last)\\n\\n\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(Df = c(NA, 0L), Deviance = c(NA, 0), `Resid. Df` = c(10L, 10L), `Resid. Dev` = c(2.74035772634541, 2.74035772634541)), .Names = c(\"Df\", \"Deviance\", \"Resid. Df\", \"Resid. Dev\"), row.names = c(\"NULL\", \"x\"), heading = \"Analysis of Deviance Table\\n\\nModel: gaussian, link: identity\\n\\nResponse: y\\n\\nTerms added sequentially (first to last)\\n\\n\"))"));     
do.call(`unclass`, argv);     
}, o=expected);     

