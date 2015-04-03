expected <- eval(parse(text="structure(list(y = NULL), .Names = \"y\", class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ 0))"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(list(y = c(-0.667819876370237, 0.170711734013213, 0.552921941721332, -0.253162069270378, -0.00786394222146348, 0.0246733498130512, 0.0730305465518564, -1.36919169254062, 0.0881443844426084, -0.0834190388782434)), .Names = \"y\", class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ 0)), structure(list(y = NULL), .Names = \"y\", class = \"data.frame\", row.names = c(NA, 10L), terms = quote(y ~ 0)))"));    
.Internal(`copyDFattr`(argv[[1]], argv[[2]]));    
}, o=expected);    

