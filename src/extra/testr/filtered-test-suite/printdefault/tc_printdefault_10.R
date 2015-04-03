expected <- eval(parse(text="structure(c(245L, 250L, 255L, 260L, 265L, 270L, 275L, 280L, 285L, 290L, 295L, 300L, 305L, 310L, 315L, 320L, 325L, 330L, 335L, 340L, 345L, 350L, 355L, 360L), .Dim = 2:4, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"), NULL))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(245L, 250L, 255L, 260L, 265L, 270L, 275L, 280L, 285L, 290L, 295L, 300L, 305L, 310L, 315L, 320L, 325L, 330L, 335L, 340L, 345L, 350L, 355L, 360L), .Dim = 2:4, .Dimnames = list(NULL, c(\"a\", \"b\", \"c\"), NULL)), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

