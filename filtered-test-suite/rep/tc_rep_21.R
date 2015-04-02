expected <- structure(c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L), class = "factor", .Label = c("Batch1", 
"Batch2"))
test(id=20, code={
argv <- list(structure(c(1L, 1L, 1L, 2L, 2L, 2L), .Label = c("Batch1", 
"Batch2"), class = "factor"), 2)
do.call('rep', argv);
},  o = expected);

