expected <- eval(parse(text="structure(c(10L, 10L, 11L, 10L, 12L, 11L, 13L, 12L, 14L, 13L, 15L, 14L, 16L, 15L, 17L, 16L, 18L, 17L, 19L, 18L, 20L, 19L, 21L, 20L, 22L, 21L, 23L, 22L, 24L, 23L, 25L, 24L, 26L, 25L, 27L, 26L, 28L, 27L, 29L, 28L, 30L, 29L, 31L, 30L, 32L, 31L, 33L, 32L, 34L, 33L, 35L, 34L, 36L, 35L, 37L, 36L, 38L, 36L, 39L, 38L, 40L, 39L), .Dim = c(2L, 31L), .Dimnames = list(c(\"target\", \"actual\"), NULL))"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(10L, 10L, 11L, 10L, 12L, 11L, 13L, 12L, 14L, 13L, 15L, 14L, 16L, 15L, 17L, 16L, 18L, 17L, 19L, 18L, 20L, 19L, 21L, 20L, 22L, 21L, 23L, 22L, 24L, 23L, 25L, 24L, 26L, 25L, 27L, 26L, 28L, 27L, 29L, 28L, 30L, 29L, 31L, 30L, 32L, 31L, 33L, 32L, 34L, 33L, 35L, 34L, 36L, 35L, 37L, 36L, 38L, 36L, 39L, 38L, 40L, 39L), c(2L, 31L), list(c(\"target\", \"actual\"), NULL))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

