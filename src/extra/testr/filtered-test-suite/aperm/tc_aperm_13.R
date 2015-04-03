expected <- eval(parse(text="structure(c(\"    Null deviance:\", \"3.118557\", \" on\", \"8\", \" degrees of freedom\\n\", \"Residual deviance:\", \"0.012672\", \" on\", \"7\", \" degrees of freedom\\n\"), .Dim = c(5L, 2L), .Dimnames = list(NULL, c(\"null.deviance\", \"deviance\")))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(\"    Null deviance:\", \"Residual deviance:\", \"3.118557\", \"0.012672\", \" on\", \" on\", \"8\", \"7\", \" degrees of freedom\\n\", \" degrees of freedom\\n\"), .Dim = c(2L, 5L), .Dimnames = list(c(\"null.deviance\", \"deviance\"), NULL)), c(2L, 1L), TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

