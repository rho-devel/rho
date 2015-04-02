expected <- eval(parse(text="structure(c(78.7365206866197, 17, 98.5088731171753, 18, 113.842206450509, 18, 123.008873117175, 18), .Dim = c(2L, 4L), .Dimnames = list(c(\"\", \"rep\"), c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(78.7365206866197, 17, 98.5088731171753, 18, 113.842206450509, 18, 123.008873117175, 18), .Dim = c(2L, 4L), .Dimnames = list(c(\"\", \"rep\"), c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\"))), 4L, TRUE, NULL, NULL, FALSE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

