expected <- eval(parse(text="structure(c(\"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\", \"\"), .Dim = c(4L, 3L), .Dimnames = list(c(\"<none>\", \"Hair:Eye\", \"Hair:Sex\", \"Eye:Sex\"), c(\"Df\", \"Deviance\", \"AIC\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"\", c(4L, 3L), list(c(\"<none>\", \"Hair:Eye\", \"Hair:Sex\", \"Eye:Sex\"), c(\"Df\", \"Deviance\", \"AIC\")))"));     
.Internal(`array`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

