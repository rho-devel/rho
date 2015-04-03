expected <- eval(parse(text="structure(c(0, -10, 0, -10, -10, 0, NA, NA, 0, 0, 0, 0, 0, 150, 0, 170, 180, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 310, 0, 330, 340, 0, 0, 350, 0, 370, 380, 0), .Dim = c(6L, 8L), .Dimnames = list(NULL, NULL))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(0, -10, 0, -10, -10, 0, NA, NA, 0, 0, 0, 0, 0, 150, 0, 170, 180, 0, 0, 0, NA, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 310, 0, 330, 340, 0, 0, 350, 0, 370, 380, 0), .Dim = c(6L, 8L), .Dimnames = list(NULL, NULL)), 1:2, TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

