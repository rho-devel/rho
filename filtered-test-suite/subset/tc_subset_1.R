expected <- eval(parse(text="structure(list(Rao = c(NA, 5.17320176026795)), .Names = \"Rao\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(`Resid. Df` = c(4, 0), `Resid. Dev` = c(5.12914107700115, 7.54951656745095e-15), Df = c(NA, 4), Deviance = c(NA, 5.12914107700114), Rao = c(NA, 5.17320176026795)), .Names = c(\"Resid. Df\", \"Resid. Dev\", \"Df\", \"Deviance\", \"Rao\"), row.names = c(\"1\", \"2\"), class = \"data.frame\"), 5L)"));                
do.call(`.subset`, argv);                
}, o=expected);                

