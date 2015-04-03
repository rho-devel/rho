expected <- c("1", "2")
test(id=0, code={
argv <- structure(list(x = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", 
"2"), class = "factor")), .Names = "x")
do.call('levels', argv);
},  o = expected);

