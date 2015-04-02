expected <- eval(parse(text="59.1153846153846"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(135L, 49L, 32L, NA, 64L, 40L, 77L, 97L, 97L, 85L, NA, 10L, 27L, NA, 7L, 48L, 35L, 61L, 79L, 63L, 16L, NA, NA, 80L, 108L, 20L, 52L, 82L, 50L, 64L, 59L), .Dim = c(31L, 1L)), 31, 1, TRUE)"));         
.Internal(colMeans(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));         
}, o=expected);         

