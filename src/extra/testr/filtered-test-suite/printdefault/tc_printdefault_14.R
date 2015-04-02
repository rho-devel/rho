expected <- eval(parse(text="structure(1:120, .Dim = 2:5, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"), NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(1:120, .Dim = 2:5, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"), NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\"))), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

