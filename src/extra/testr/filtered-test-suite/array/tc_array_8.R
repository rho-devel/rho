expected <- eval(parse(text="structure(c(NA, NA, NA, NA, NA, NA, 29L, NA, 71L, 39L, NA, NA, 23L, NA, NA, 21L, 37L, 20L, 12L, 13L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(30L, 1L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(NA, NA, NA, NA, NA, NA, 29L, NA, 71L, 39L, NA, NA, 23L, NA, NA, 21L, 37L, 20L, 12L, 13L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), c(30L, 1L), NULL)"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

