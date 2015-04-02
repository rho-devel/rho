expected <- structure(c(NA, 1L, 4L, 4L, 6L, 4L, 5L, 5L, NA, 6L, 6L, NA, 3L, 
6L, 4L, 2L, 1L, 6L, 1L, 3L, 3L, 5L, 2L, 2L, 2L, 5L, 3L, 3L, 1L, 
2L, 5L, 6L, 6L, 6L, 6L, 2L, NA, 1L, 5L, 1L, 2L, NA, 4L, NA, 5L, 
5L, 2L, 6L, 4L, NA, NA, 1L, 2L, 5L, 1L, 1L, 4L, 3L, 3L, 4L, 4L, 
2L, 5L, 3L, NA, 5L, NA, 6L, NA, 5L, 2L, 6L, 2L, 4L, 2L, 2L, 4L, 
4L, 1L, 6L, 2L, 1L, 5L, NA, 5L, 1L, 2L, 2L, 4L, 2L, 4L, 2L, 5L, 
6L, 5L, 6L, 3L, 1L, 2L, 4L, NA, 6L, 3L, 3L, 2L, 6L, 2L, 5L, NA, 
NA, 3L, 4L, 6L, 3L, 4L, 2L, NA, 1L, 6L, 2L, 4L, 4L, 1L, NA, 4L, 
3L, 4L, NA, 4L, NA, NA, 5L, 5L, 5L, 4L, 4L, NA, 2L, 6L, 3L, 2L, 
1L, 1L, 6L, 2L, 2L, 5L, 1L, 5L, 3L, 2L, 2L, 5L, NA, 6L, 3L, NA, 
4L, 2L, 2L, 5L, 6L, 6L, 1L, 1L, 6L, 6L, NA, 2L, 5L, NA, 5L, 4L, 
NA, 2L, 5L, 4L, 3L, 5L, 1L, NA, 4L, 4L, 3L, 1L, 1L, 5L, 4L, NA, 
3L, 5L, NA, 5L, NA, 6L, 6L, 2L, 4L, 3L, 3L), .Label = c("a", 
"b", "c", "d", "e", "f"), class = "factor")
test(id=0, code={
argv <- structure(list(x = structure(c(4L, 1L, 4L, 4L, 6L, 4L, 5L, 5L, 
4L, 6L, 6L, 2L, 3L, 6L, 4L, 2L, 1L, 6L, 1L, 3L, 3L, 5L, 2L, 2L, 
2L, 5L, 3L, 3L, 1L, 2L, 5L, 6L, 6L, 6L, 6L, 2L, 6L, 1L, 5L, 1L, 
2L, 4L, 4L, 6L, 5L, 5L, 2L, 6L, 4L, 6L, 5L, 1L, 2L, 5L, 1L, 1L, 
4L, 3L, 3L, 4L, 4L, 2L, 5L, 3L, 4L, 5L, 4L, 6L, 4L, 5L, 2L, 6L, 
2L, 4L, 2L, 2L, 4L, 4L, 1L, 6L, 2L, 1L, 5L, 3L, 5L, 1L, 2L, 2L, 
4L, 2L, 4L, 2L, 5L, 6L, 5L, 6L, 3L, 1L, 2L, 4L, 6L, 6L, 3L, 3L, 
2L, 6L, 2L, 5L, 3L, 4L, 3L, 4L, 6L, 3L, 4L, 2L, 3L, 1L, 6L, 2L, 
4L, 4L, 1L, 3L, 4L, 3L, 4L, 1L, 4L, 1L, 3L, 5L, 5L, 5L, 4L, 4L, 
6L, 2L, 6L, 3L, 2L, 1L, 1L, 6L, 2L, 2L, 5L, 1L, 5L, 3L, 2L, 2L, 
5L, 1L, 6L, 3L, 6L, 4L, 2L, 2L, 5L, 6L, 6L, 1L, 1L, 6L, 6L, 5L, 
2L, 5L, 6L, 5L, 4L, 6L, 2L, 5L, 4L, 3L, 5L, 1L, 3L, 4L, 4L, 3L, 
1L, 1L, 5L, 4L, 1L, 3L, 5L, 4L, 5L, 4L, 6L, 6L, 2L, 4L, 3L, 3L
), .Label = c("a", "b", "c", "d", "e", "f"), class = "factor"), 
    c(189L, 84L, 154L, 9L, 130L, 44L, 137L, 12L, 50L, 1L, 42L, 
    174L, 194L, 131L, 157L, 101L, 37L, 128L, 117L, 181L, 51L, 
    109L, 110L, 67L, 69L, 124L, 192L, 65L, 171L, 168L), value = NA), .Names = c("x", 
"", "value"))
do.call('[<-.factor', argv);
},  o = expected);

