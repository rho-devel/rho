expected <- eval(parse(text="structure(c(\"-0.91\", \" 0.81\", \"\", \"-0.97\"), .Dim = c(2L, 2L), .Dimnames = list(c(\"x1\", \"x3\"), c(\"(Intercept)\", \"x1\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"-0.91\", \" 0.81\", \"\", \"-0.97\"), .Dim = c(2L, 2L), .Dimnames = list(c(\"x1\", \"x3\"), c(\"(Intercept)\", \"x1\"))), NULL, FALSE, NULL, NULL, FALSE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

