expected <- eval(parse(text="structure(c(495L, 515L, 535L, 555L, 575L, 595L, 615L, 635L, 655L, 675L, 695L, 715L), .Dim = 3:4, .Dimnames = list(c(\"a\", \"b\", \"c\"), NULL))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(495L, 515L, 535L, 555L, 575L, 595L, 615L, 635L, 655L, 675L, 695L, 715L), .Dim = 3:4, .Dimnames = list(c(\"a\", \"b\", \"c\"), NULL)), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

